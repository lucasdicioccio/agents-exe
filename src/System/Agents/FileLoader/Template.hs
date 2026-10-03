{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Agent files written as tramaj programs.

A @.tramaj@ agent file is a program that evaluates, once, at load, in
concrete mode, to the same JSON value a @.json@ agent file holds. The value
then goes through the ordinary 'AgentDescription' parser, so everything
downstream of loading sees a plain agent.

The program's @$ctx@ is the operator's process parameters (@--set@,
@--set-json@, @--pin@, @--pin-json@, @--params-file@). A template declares
what it reads the same way an agent declares what its bindings read: every
@$ctx.NAME@ must be a process-scope, non-secret entry of the evaluated
agent's @parameters@. A read of anything else is refused at load, as an
undeclared binding is.

Libraries are other tramaj programs a template reaches with
@import("name", {...})@. 'standardLibraries' holds the one agents-exe
ships, named @agents@; 'loadLibraryDirectories' adds the operator's.
-}
module System.Agents.FileLoader.Template (
    -- * Libraries
    TemplateLibraries,
    standardLibraries,
    standardLibraryName,
    standardLibrarySource,
    parseLibrary,
    loadLibraryDirectory,
    loadLibraryDirectories,

    -- * Evaluation
    TemplateEnv (..),
    defaultTemplateEnv,
    TemplateError (..),
    renderTemplateError,
    evalTemplate,
    evalAgentTemplate,
    readTemplateDescriptionFile,

    -- * File names
    templateExtension,
    isTemplateFile,
) where

import Control.Exception (IOException, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, (</>))
import Text.Megaparsec (errorBundlePretty)

import qualified Tramaj.Analysis as Tramaj
import Tramaj.Ast (Program)
import qualified Tramaj.Eval as Tramaj
import qualified Tramaj.Parser as Tramaj

import System.Agents.Base (Agent (..), AgentDescription (..))
import System.Agents.Tools.Params.Types (
    ParamName,
    ParamScope (..),
    ParameterDecl (..),
    ProcessParams,
    ProcessValue (..),
 )

-------------------------------------------------------------------------------
-- File names
-------------------------------------------------------------------------------

-- | The extension of an agent template and of a library file: @.tramaj@.
templateExtension :: String
templateExtension = ".tramaj"

isTemplateFile :: FilePath -> Bool
isTemplateFile p = takeExtension p == templateExtension

-------------------------------------------------------------------------------
-- Libraries
-------------------------------------------------------------------------------

-- | Parsed libraries, by the name an @import("name", {...})@ gives.
type TemplateLibraries = Map Text Program

-- | The name of the library agents-exe ships.
standardLibraryName :: Text
standardLibraryName = "agents"

{- | The source of the @agents@ library: constructors for the JSON shapes an
agent file is made of, so that templates share sandboxes and toolboxes by
reference instead of repeating them. Read as
@\@a=import("agents", {}).vals@, then @$a.sandbox({...})@.
-}
standardLibrarySource :: Text
standardLibrarySource =
    Text.unlines
        [ "-- agents-exe standard library for agent templates."
        , "--   @a=import(\"agents\", {}).vals"
        , ""
        , "-- A field the caller may leave out: {} unless spec has key."
        , "@opt=(spec, key, wrap) => branch({}, has($spec, $key), $wrap(lookup($spec, $key, null)))"
        , ""
        , "-- Path predicates."
        , "@dir=(path) => {tag: \"DirectoryRecursive\", contents: $path}"
        , "@dir-shallow=(path) => {tag: \"DirectoryShallow\", contents: $path}"
        , "@dir-exactly=(path) => {tag: \"DirectoryExactly\", contents: $path}"
        , "@file=(path) => {tag: \"FileExactly\", contents: $path}"
        , "@pattern=(glob) => {tag: \"FilePattern\", contents: $glob}"
        , "@extensions=(exts) => {tag: \"FileExtension\", contents: $exts}"
        , "@any-of=(preds) => {tag: \"Any\", contents: $preds}"
        , "@all-of=(preds) => {tag: \"All\", contents: $preds}"
        , "@none-of=(preds) => {tag: \"Not\", contents: {tag: \"Any\", contents: $preds}}"
        , "@allow-all={tag: \"AlwaysAllow\"}"
        , "@deny-all={tag: \"AlwaysDeny\"}"
        , "-- Each path below a root, as recursive directories."
        , "@under=(root, paths) => map($paths, (p) => {tag: \"DirectoryRecursive\", contents: \"`$root`/`$p`\"})"
        , ""
        , "-- sandbox({allow: [predicate], deny: [predicate], name, maxFileSize})"
        , "-- allow is required; a path is allowed when one allow entry matches"
        , "-- and no deny entry does."
        , "@sandbox=(spec) => ("
        , "  {fsbPredicate: branch("
        , "     {tag: \"Any\", contents: $spec.allow},"
        , "     has($spec, \"deny\"),"
        , "     {tag: \"And\", contents: ["
        , "        {tag: \"Any\", contents: $spec.allow},"
        , "        {tag: \"Not\", contents: {tag: \"Any\", contents: lookup($spec, \"deny\", [])}}]})}"
        , "  <> $opt($spec, \"name\", (v) => {fsbName: $v})"
        , "  <> $opt($spec, \"maxFileSize\", (v) => {fsbMaxFileSize: $v}))"
        , ""
        , "-- Builtin toolboxes. name and capabilities are required."
        , "@developer-toolbox=(spec) => {tag: \"DeveloperToolbox\", contents: ("
        , "  {Name: $spec.name,"
        , "   Description: lookup($spec, \"description\", \"Development tools\"),"
        , "   Capabilities: $spec.capabilities}"
        , "  <> $opt($spec, \"sandbox\", (v) => {FileSandbox: $v})"
        , "  <> $opt($spec, \"activation\", (v) => {Activation: $v})"
        , "  <> $opt($spec, \"buildCommand\", (v) => {BuildCommand: $v}))}"
        , "@system-toolbox=(spec) => {tag: \"SystemToolbox\", contents: ("
        , "  {Name: $spec.name,"
        , "   Description: lookup($spec, \"description\", \"System context\"),"
        , "   Capabilities: $spec.capabilities}"
        , "  <> $opt($spec, \"sandbox\", (v) => {FileSandbox: $v})"
        , "  <> $opt($spec, \"activation\", (v) => {Activation: $v})"
        , "  <> $opt($spec, \"envVarFilter\", (v) => {EnvVarFilter: $v})"
        , "  <> $opt($spec, \"commandFilter\", (v) => {CommandFilter: $v}))}"
        , "@lua-toolbox=(spec) => {tag: \"LuaToolbox\", contents: ("
        , "  {Name: $spec.name,"
        , "   Description: lookup($spec, \"description\", \"Lua orchestration\"),"
        , "   MaxMemoryMB: lookup($spec, \"maxMemoryMB\", 256),"
        , "   MaxExecutionTimeSeconds: lookup($spec, \"maxExecutionTimeSeconds\", 300),"
        , "   AllowedTools: lookup($spec, \"allowedTools\", []),"
        , "   AllowedHosts: lookup($spec, \"allowedHosts\", [])}"
        , "  <> $opt($spec, \"sandbox\", (v) => {FileSandbox: $v})"
        , "  <> $opt($spec, \"activation\", (v) => {Activation: $v}))}"
        , ""
        , "-- Bash tools: a directory of them, or one."
        , "@tools-dir=(spec) => {tag: \"FileSystemDirectory\", contents: ("
        , "  {Path: $spec.path}"
        , "  <> $opt($spec, \"filter\", (v) => {BasenameFilter: $v})"
        , "  <> $opt($spec, \"activation\", (v) => {Activation: $v})"
        , "  <> $opt($spec, \"bindings\", (v) => {Bindings: $v}))}"
        , "@tool=(spec) => {tag: \"SingleTool\", contents: ("
        , "  {Path: $spec.path}"
        , "  <> $opt($spec, \"activation\", (v) => {Activation: $v})"
        , "  <> $opt($spec, \"bindings\", (v) => {Bindings: $v}))}"
        , ""
        , "-- mcp({name, executable, args, env})"
        , "@mcp=(spec) => {tag: \"McpSimpleBinary\", contents: ("
        , "  {name: $spec.name,"
        , "   executable: $spec.executable,"
        , "   args: lookup($spec, \"args\", [])}"
        , "  <> $opt($spec, \"activation\", (v) => {Activation: $v})"
        , "  <> $opt($spec, \"env\", (v) => {env: $v}))}"
        , ""
        , "-- helper({slug, path, with, narrowable}): an extraAgents entry."
        , "@helper=(spec) => ("
        , "  {slug: $spec.slug, path: $spec.path}"
        , "  <> $opt($spec, \"with\", (v) => {with: $v})"
        , "  <> $opt($spec, \"narrowable\", (v) => {narrowable: $v}))"
        , ""
        , "-- The envelope an agent file is: agent({slug, ...})."
        , "@agent=(contents) => {tag: \"OpenAIAgentDescription\", contents: $contents}"
        , ""
        , "null"
        ]

{- | The libraries every template can import: the @agents@ one. A parse
failure here is a bug in agents-exe (the test suite evaluates it).
-}
standardLibraries :: TemplateLibraries
standardLibraries =
    case parseLibrary standardLibrarySource of
        Right prog -> Map.singleton standardLibraryName prog
        Left err -> error ("agents-exe: the built-in tramaj library does not parse: " <> err)

-- | Parse one library's source.
parseLibrary :: Text -> Either String Program
parseLibrary src =
    either (Left . errorBundlePretty) Right (Tramaj.parseProgram src)

{- | Every @.tramaj@ file of a directory as a library named after the file
(@sandboxes.tramaj@ is @import("sandboxes", {...})@). A directory that does
not exist holds no library.
-}
loadLibraryDirectory :: FilePath -> IO (Either String TemplateLibraries)
loadLibraryDirectory path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure (Right Map.empty)
        else do
            entries <- List.sort . filter isTemplateFile <$> listDirectory path
            results <- mapM loadOne entries
            pure (Map.fromList <$> sequence results)
  where
    loadOne entry = do
        let file = path </> entry
        src <- try (Text.IO.readFile file) :: IO (Either IOException Text)
        pure $ case src of
            Left e -> Left (file <> ": " <> show e)
            Right txt -> case parseLibrary txt of
                Left err -> Left (file <> ": " <> err)
                Right prog -> Right (Text.pack (dropExtension entry), prog)

{- | The libraries of several directories, over 'standardLibraries'. On a
name defined twice, the earlier directory wins, and any directory wins over
the standard library.
-}
loadLibraryDirectories :: [FilePath] -> IO (Either String TemplateLibraries)
loadLibraryDirectories paths = do
    results <- mapM loadLibraryDirectory paths
    pure $ case sequence results of
        Left err -> Left err
        Right tables -> Right (Map.unions (tables <> [standardLibraries]))

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

-- | What a template is evaluated with.
data TemplateEnv = TemplateEnv
    { teLibraries :: TemplateLibraries
    , teParams :: ProcessParams
    -- ^ Becomes the program's @$ctx@, one field per parameter.
    }

-- | The standard library and no parameter.
defaultTemplateEnv :: TemplateEnv
defaultTemplateEnv = TemplateEnv standardLibraries mempty

data TemplateError
    = TemplateParseError String
    | -- | The program uses @$ctx@ as a whole; a parameter is read by name.
      TemplateReadsWholeContext
    | -- | @$ctx.NAME@ is read and the operator supplied no value for it.
      TemplateParamNotSupplied ParamName
    | TemplateEvalError Tramaj.EvalError
    | -- | The program evaluated to a document, not a JSON value.
      TemplateNotAValue
    | -- | The value is not an agent description.
      TemplateNotAnAgent String
    | -- | Parameters read and not declared in the agent's @parameters@.
      TemplateUndeclaredParams [ParamName]
    | -- | Parameters read and declared with a scope other than @process@.
      TemplateNonProcessParams [ParamName]
    | -- | Parameters read and declared @secret@.
      TemplateSecretParams [ParamName]
    deriving (Show, Eq)

renderTemplateError :: TemplateError -> String
renderTemplateError = \case
    TemplateParseError err -> "tramaj parse error:\n" <> err
    TemplateReadsWholeContext ->
        "the template uses $ctx as a whole; read each parameter by name ($ctx.NAME) so that it can be checked against the agent's declared parameters"
    TemplateParamNotSupplied name ->
        "the template reads parameter '" <> Text.unpack name <> "', which has no value; supply one with --set " <> Text.unpack name <> "=VALUE (or --set-json, --pin, --params-file)"
    TemplateEvalError err -> "tramaj evaluation error: " <> renderEvalError err
    TemplateNotAValue -> "the template evaluates to a document; an agent template must evaluate to a JSON object"
    TemplateNotAnAgent err -> "the template does not evaluate to an agent: " <> err
    TemplateUndeclaredParams names ->
        "the template reads " <> listed names <> ", not declared in the agent's \"parameters\""
    TemplateNonProcessParams names ->
        "the template reads " <> listed names <> ", declared with a scope other than \"process\"; a template is evaluated once at load and can only read process-scope parameters"
    TemplateSecretParams names ->
        "the template reads " <> listed names <> ", declared secret; a secret value must not be written into an agent's configuration (bind it to a tool argument instead)"
  where
    listed [n] = "parameter '" <> Text.unpack n <> "'"
    listed ns = "parameters " <> List.intercalate ", " ["'" <> Text.unpack n <> "'" | n <- ns]

renderEvalError :: Tramaj.EvalError -> String
renderEvalError = \case
    Tramaj.UnboundName n -> "unbound name '" <> Text.unpack n <> "'"
    Tramaj.PathNotFound path -> "path not found: $" <> Text.unpack (Text.intercalate "." path)
    Tramaj.TypeMismatch msg -> Text.unpack msg
    Tramaj.UnknownLibrary n -> "unknown library '" <> Text.unpack n <> "' (libraries come from the directories listed as tramajLibraries in agents-exe.cfg.json)"
    Tramaj.ImportCycle n -> "import cycle through library '" <> Text.unpack n <> "'"
    Tramaj.InLibrary n e -> "in library '" <> Text.unpack n <> "': " <> renderEvalError e
    Tramaj.SymbolsUnavailable -> "symbols are not available: agent templates are evaluated in concrete mode"
    other -> show other

{- | Evaluate a template's source to the JSON value it denotes, without
looking at what the value is. Parameters are not checked against any
declaration here; 'evalAgentTemplate' does that.
-}
evalTemplate :: TemplateEnv -> Text -> Either TemplateError (Program, Aeson.Value)
evalTemplate env src = do
    prog <- either (Left . TemplateParseError . errorBundlePretty) Right (Tramaj.parseProgram src)
    out <- either (Left . classify) Right (Tramaj.evalProgram Tramaj.Concrete env.teLibraries ctx prog)
    case out of
        Tramaj.OValue v -> Right (prog, v)
        Tramaj.ONode _ -> Left TemplateNotAValue
  where
    ctx =
        Aeson.Object $
            Aeson.KeyMap.fromList
                [(Aeson.Key.fromText k, v.pvRawValue) | (k, v) <- Map.toList env.teParams]
    -- A root-level miss on $ctx.NAME is the operator's to fix, not the author's.
    classify (Tramaj.PathNotFound ("ctx" : name : _))
        | not (Map.member name env.teParams) = TemplateParamNotSupplied name
    classify e = TemplateEvalError e

{- | Evaluate an agent template: the JSON value it denotes and the agent that
value describes, once every parameter the program reads has been checked
against the agent's own @parameters@.
-}
evalAgentTemplate :: TemplateEnv -> Text -> Either TemplateError (Aeson.Value, AgentDescription)
evalAgentTemplate env src = do
    (prog, value) <- evalTemplate env src
    desc@(AgentDescription agent) <-
        either (Left . TemplateNotAnAgent) Right (Aeson.Types.parseEither Aeson.parseJSON value)
    let reads' = Tramaj.contextReads prog
        names = List.nub [name | (name : _) <- Set.toList reads']
        decls = Map.fromList [(d.paramName, d) | d <- fromMaybe [] agent.parameters]
        undeclared = [n | n <- names, not (Map.member n decls)]
        nonProcess = [n | n <- names, Just d <- [Map.lookup n decls], d.paramScope /= ScopeProcess]
        secret = [n | n <- names, Just d <- [Map.lookup n decls], d.paramSecret]
    if Set.member [] reads'
        then Left TemplateReadsWholeContext
        else case (undeclared, nonProcess, secret) of
            ([], [], []) -> Right (value, desc)
            (_ : _, _, _) -> Left (TemplateUndeclaredParams undeclared)
            (_, _ : _, _) -> Left (TemplateNonProcessParams nonProcess)
            (_, _, _) -> Left (TemplateSecretParams secret)

-- | Read and evaluate an agent template file.
readTemplateDescriptionFile :: TemplateEnv -> FilePath -> IO (Either String (Aeson.Value, AgentDescription))
readTemplateDescriptionFile env path = do
    src <- try (Text.IO.readFile path) :: IO (Either IOException Text)
    pure $ case src of
        Left e -> Left (show e)
        Right txt -> either (Left . renderTemplateError) Right (evalAgentTemplate env txt)
