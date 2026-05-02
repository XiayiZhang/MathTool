module StringFunc where

import Data.List (isPrefixOf, tails, stripPrefix)
import Data.Maybe (listToMaybe)

--ai写的
findSubstr :: String -> String -> Maybe String
findSubstr pat s = listToMaybe [t | t <- tails s, pat `isPrefixOf` t]

findBetween :: String -> String -> String -> Maybe String
findBetween start end s = do
    afterStart <- findSubstr start s
    rest <- stripPrefix start afterStart
    endPos <- findSubstr end rest
    return (take (length rest - length endPos) rest)
--
