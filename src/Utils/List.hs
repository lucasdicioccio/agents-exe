-- | Safe list utility functions
module Utils.List (
    safeHead,
    safeLast,
    safeTail,
    chunksOf,
) where

import Data.List (unfoldr)

{- | Safely get the first element of a list.

>>> safeHead [1, 2, 3]
Just 1
>>> safeHead []
Nothing
-}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

{- | Safely get the last element of a list.

>>> safeLast [1, 2, 3]
Just 3
>>> safeLast []
Nothing
-}
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

{- | Safely get the tail of a list.

>>> safeTail [1, 2, 3]
Just [2,3]
>>> safeTail []
Nothing
-}
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

{- | Split a list into chunks of a specified size.

>>> chunksOf 3 [1..10]
[[1,2,3],[4,5,6],[7,8,9],[10]]
>>> chunksOf 0 [1, 2, 3]
[]
-}
chunksOf :: Int -> [a] -> [[a]]
chunksOf n
    | n <= 0 = const []
    | otherwise =
        unfoldr
            ( \xs -> case splitAt n xs of
                ([], _) -> Nothing
                res -> Just res
            )
