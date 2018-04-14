module Utility where

import Data.Map.Strict  as Map (Map, lookup, insert)

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

isJust :: Maybe a -> Bool
isJust = not . isNothing

formJust :: Maybe a -> a
formJust (Just a) =  a
fromJust Nothing = undefined

removeSpace :: String -> String
removeSpace = foldr f "" where
    f c acc | c == ' ' || c == '\t' || c == '\r' = acc
            | otherwise = c : acc

removeSpaces :: [String] -> [String]
removeSpaces = map f where
    f' c acc | c == ' ' || c == '\t' || c == '\r' = acc
             | otherwise = c : acc
    f = foldr f' ""

insertIfAbsent :: Ord k => k -> a -> Map k a -> Map k a
insertIfAbsent k a m = case Map.lookup k m of
    Just old  -> m
    Nothing -> insert k a m
