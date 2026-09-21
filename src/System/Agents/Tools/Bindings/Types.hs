{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure data types for partial application of tool arguments.

Kept in a leaf module (no dependency on 'ToolRegistration' or any toolbox)
so that toolbox description types in "System.Agents.Base" can carry a
@Bindings@ field without creating an import cycle. The combinator that
actually applies these to a 'ToolRegistration' lives in
"System.Agents.Tools.Bindings".
-}
module System.Agents.Tools.Bindings.Types (
    Binding (..),
    BindingValue (..),
    WhenUnbound (..),
    ToolGlob,
    globMatches,
    bindingsForTool,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- | A glob against a toolbox-local or LLM-visible tool name. @*@ matches any run of characters.
type ToolGlob = Text

{- | What a binding resolves to. Phase 1 only implements 'Literal'; 'Param'
(a reference to an agent parameter, resolved per call) is added in Phase 2.
-}
data BindingValue = Literal Value
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON BindingValue where
    toJSON (Literal v) = Aeson.object ["tag" Aeson..= ("Literal" :: Text), "contents" Aeson..= v]

instance Aeson.FromJSON BindingValue where
    parseJSON = Aeson.withObject "BindingValue" $ \o -> do
        tag <- o Aeson..: "tag"
        case (tag :: Text) of
            "Literal" -> Literal <$> o Aeson..: "contents"
            _ -> fail "expecting 'Literal' tag (only tag supported so far)"

-- | What happens when a binding's value cannot be resolved at call time.
data WhenUnbound
    = Fail
    | Omit
    | Expose
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON WhenUnbound where
    toJSON Fail = Aeson.String "fail"
    toJSON Omit = Aeson.String "omit"
    toJSON Expose = Aeson.String "expose"

instance Aeson.FromJSON WhenUnbound where
    parseJSON (Aeson.String "fail") = pure Fail
    parseJSON (Aeson.String "omit") = pure Omit
    parseJSON (Aeson.String "expose") = pure Expose
    parseJSON _ = fail "expecting one of 'fail', 'omit', 'expose'"

-- | A binding of one tool argument to a fixed value.
data Binding = Binding
    { bindTool :: Maybe ToolGlob
    -- ^ Which tools this applies to. 'Nothing' means every tool in the toolbox.
    , bindArg :: Text
    , bindValue :: BindingValue
    , bindWhenUnbound :: WhenUnbound
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON Binding where
    toJSON b =
        Aeson.object $
            maybe [] (\t -> ["tool" Aeson..= t]) b.bindTool
                ++ [ "arg" Aeson..= b.bindArg
                   , "value" Aeson..= b.bindValue
                   , "whenUnbound" Aeson..= b.bindWhenUnbound
                   ]

instance Aeson.FromJSON Binding where
    parseJSON = Aeson.withObject "Binding" $ \o ->
        Binding
            <$> o Aeson..:? "tool"
            <*> o Aeson..: "arg"
            <*> o Aeson..: "value"
            <*> (maybe Fail id <$> o Aeson..:? "whenUnbound")

{- | Matches a glob against a name. @*@ matches any run of characters
(including none); everything else is matched literally. No other glob
syntax (no @?@, no character classes) is supported.
-}
globMatches :: ToolGlob -> Text -> Bool
globMatches glob name = go (Text.unpack glob) (Text.unpack name)
  where
    go [] [] = True
    go ('*' : ps) s = go ps s || (not (null s) && go ('*' : ps) (drop 1 s))
    go (p : ps) (c : cs) = p == c && go ps cs
    go _ _ = False

-- | The bindings that apply to one LLM-visible tool name.
bindingsForTool :: Text -> [Binding] -> [Binding]
bindingsForTool name = filter (\b -> maybe True (`globMatches` name) b.bindTool)
