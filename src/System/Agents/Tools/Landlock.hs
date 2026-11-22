{- | The Landlock sandbox mechanism for bash tools (@--sandbox landlock@,
@todos/bash-tools-v2.md@ §1.4).

Landlock is a Linux security module that needs no privilege and no
namespaces: a process restricts itself, and the restriction survives @exec@
and is inherited by its children. The rules have to be applied in the child
before it execs the tool, and the process library offers no hook there, so
agents-exe runs a sandboxed tool through itself: the command is

> agents-exe __agents-exe-landlock-exec --read P ... --write P ... -- tool run ...

and the program's @main@ calls 'landlockLauncherHook' first, which notices
the marker argument, applies the rules and execs the tool
(@cbits/landlock_exec.c@). A program that runs Landlock tools must call the
hook; one that does not fails to start the tool (its own option parser
refuses the marker), it never runs it unconfined.

Paths outside the rules are denied, not hidden; the network is not touched.
-}
module System.Agents.Tools.Landlock (
    landlockAbi,
    LandlockRule (..),
    LandlockAccess (..),
    landlockLauncherArg,
    landlockLauncherArgs,
    landlockLauncherHook,
    runLandlockLauncher,
) where

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArray0, withArrayLen)
import Foreign.Ptr (Ptr, nullPtr)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

foreign import ccall unsafe "agents_landlock_abi"
    c_landlock_abi :: IO CInt

foreign import ccall unsafe "agents_landlock_exec"
    c_landlock_exec :: CInt -> Ptr CString -> Ptr CInt -> CString -> Ptr CString -> CString -> CSize -> IO CInt

{- | The Landlock ABI version of the running kernel, or why there is none
(not built in, or not enabled in the @lsm=@ boot parameter).
-}
landlockAbi :: IO (Either String Int)
landlockAbi = do
    r <- c_landlock_abi
    pure $
        if r >= 1
            then Right (fromIntegral r)
            else Left $ case negate r of
                38 -> "the kernel has no Landlock support (ENOSYS)"
                95 -> "Landlock is not enabled on this kernel (EOPNOTSUPP: it is missing from the lsm= boot parameter)"
                e -> "Landlock is unavailable (errno " <> show e <> ")"

data LandlockAccess
    = -- | Read files, list directories, execute.
      AccessRead
    | -- | Every access Landlock handles on this kernel.
      AccessWrite
    deriving (Show, Eq, Ord)

-- | Access to a path and everything beneath it.
data LandlockRule = LandlockRule
    { ruleAccess :: LandlockAccess
    , rulePath :: FilePath
    , ruleOptional :: Bool
    -- ^ Skip the rule when the path does not exist (system paths); a
    -- missing required path fails the launch.
    }
    deriving (Show, Eq, Ord)

-- | The first argument that turns a program into the launcher.
landlockLauncherArg :: String
landlockLauncherArg = "__agents-exe-landlock-exec"

{- | The launcher's arguments for the rules and the command (the executable
of the program comes before them).
-}
landlockLauncherArgs :: [LandlockRule] -> FilePath -> [String] -> [String]
landlockLauncherArgs rules cmd args =
    landlockLauncherArg : concatMap ruleArgs rules <> ("--" : cmd : args)
  where
    ruleArgs :: LandlockRule -> [String]
    ruleArgs r = [flag r, r.rulePath]
    flag :: LandlockRule -> String
    flag r = case (r.ruleAccess, r.ruleOptional) of
        (AccessRead, False) -> "--read"
        (AccessRead, True) -> "--read-try"
        (AccessWrite, False) -> "--write"
        (AccessWrite, True) -> "--write-try"

{- | Call first in @main@ of every program that may run a Landlock-sandboxed
tool: when the program was started as the launcher, this applies the rules
and execs the tool, and never returns. Otherwise it does nothing.
-}
landlockLauncherHook :: IO ()
landlockLauncherHook = do
    args <- getArgs
    case args of
        (a : rest) | a == landlockLauncherArg -> runLandlockLauncher rest
        _ -> pure ()

{- | Parses the launcher's arguments, confines this process and execs the
command. Exits 126 (the shell's "cannot execute") with a message on stderr
when that fails.
-}
runLandlockLauncher :: [String] -> IO a
runLandlockLauncher args0 = do
    (rules, cmdArgs) <- either die' pure (parse [] args0)
    cmd <- case cmdArgs of
        [] -> die' "no command after --"
        (c : _) -> pure c
    let paths = fmap (.rulePath) rules
        modes = fmap mode rules
    r <-
        withMany withCString paths $ \cPaths ->
            withArrayLen cPaths $ \n pathsPtr ->
                withArray0 0 modes $ \modesPtr ->
                    withCString cmd $ \cCmd ->
                        withMany withCString cmdArgs $ \cArgv ->
                            withArray0 nullPtr cArgv $ \argvPtr ->
                                allocaBytes errLen $ \errBuf -> do
                                    _ <- c_landlock_exec (fromIntegral n) pathsPtr modesPtr cCmd argvPtr errBuf (fromIntegral errLen)
                                    peekCString errBuf
    die' r
  where
    errLen = 1024
    mode :: LandlockRule -> CInt
    mode rule = (if rule.ruleAccess == AccessWrite then 1 else 0) + (if rule.ruleOptional then 2 else 0)
    parse acc xs = case xs of
        ("--" : rest) -> Right (reverse acc, rest)
        (flag : p : rest) | Just mk <- lookup flag flags -> parse (mk p : acc) rest
        (other : _) -> Left ("unexpected argument " <> show other)
        [] -> Left "missing -- before the command"
    flags =
        [ ("--read", \p -> LandlockRule AccessRead p False)
        , ("--read-try", \p -> LandlockRule AccessRead p True)
        , ("--write", \p -> LandlockRule AccessWrite p False)
        , ("--write-try", \p -> LandlockRule AccessWrite p True)
        ]
    die' msg = do
        hPutStrLn stderr ("agents-exe landlock sandbox: " <> msg)
        exitWith (ExitFailure 126)

withMany :: (a -> (b -> IO c) -> IO c) -> [a] -> ([b] -> IO c) -> IO c
withMany with xs k = case xs of
    [] -> k []
    (x : rest) -> with x $ \b -> withMany with rest (\bs -> k (b : bs))
