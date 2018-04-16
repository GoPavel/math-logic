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
    Nothing -> Map.insert k a m

-- p (x , old) = 1 => x -> map
-- p (x , old) = 0 => map
insertIfAbsentIf :: Ord k => (a -> a -> Bool) -> k -> a -> Map k a -> Map k a
insertIfAbsentIf p k a m = case Map.lookup k m of
    Nothing -> Map.insert k a m
    Just old -> if p a old
                then Map.insert k a m
                else m

splitOn2 :: (Char, Char) -> String -> [String]
splitOn2 _ [] = []
splitOn2 (a, b) xs = reverse $ func (a, b) xs [] [] where
    func :: (Char, Char) -> String -> String -> [String] -> [String]
    func (c, d) (a:b:xs) rs xss =
        if a == c && b == d
            then func (c, d) xs [] (rs : xss)
            else func (c, d) (b : xs) (rs ++ [a]) xss
    func _ [a] rs xss = (rs ++ [a]) : xss
    func _ [] rs xss = rs : xss
--TODO O(N * N) :(

splitOn1 :: Char -> String -> [String]
splitOn1 _ [] = []
splitOn1 ch xs = reverse $ func ch xs [] [] where
    func :: Char -> String -> String -> [String] -> [String]
    func ch (x:xs) rs xss =
        if x == ch
            then func ch xs [] (rs : xss)
            else func ch xs (rs ++ [x]) xss
    func _ [] rs xss = rs : xss
--TODO O(N * N) :(
