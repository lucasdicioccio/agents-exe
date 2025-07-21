module System.Agents.Runtime.Base (PingPongQuery (..), getQueryToAnswer) where

import Data.Text (Text)

-------------------------------------------------------------------------------
data PingPongQuery
    = SomeQueryToAnswer Text
    | GaveToolAnswers
    | NoQuery
    deriving (Show)

getQueryToAnswer :: PingPongQuery -> Maybe Text
getQueryToAnswer (SomeQueryToAnswer t) = Just t
getQueryToAnswer _ = Nothing
