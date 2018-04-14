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
removeSpaces = map removeSpace

insertIfAbsent :: Ord k => k -> a -> Map k a -> Map k a
insertIfAbsent k a m = case Map.lookup k m of
    Just old  -> m
    Nothing -> insert k a m

splitOn2 :: (Char, Char) -> [Char] -> [[Char]]
splitOn2 _ [] = []
splitOn2 (a, b) xs = reverse $ func (a, b) xs [] [] where
    func :: (Char, Char) -> [Char] -> [Char] -> [[Char]] -> [[Char]]
    func (c, d) (a:b:xs) rs xss =
        if a == c && b == d
            then func (c, d) xs [] (rs : xss)
            else func (c, d) (b : xs) (rs ++ [a]) xss
    func _ [a] rs xss = (rs ++ [a]) : xss
    func _ [] rs xss = rs : xss
--TODO O(N * N) :(

splitOn1 :: Char -> [Char] -> [[Char]]
splitOn1 _ [] = []
splitOn1 ch xs = reverse $ func ch xs [] [] where
    func :: Char -> [Char] -> [Char] -> [[Char]] -> [[Char]]
    func ch (x:xs) rs xss =
        if x == ch
            then func ch xs [] (rs : xss)
            else func ch xs (rs ++ [x]) xss
    func _ [] rs xss = rs : xss
--TODO O(N * N) :(
