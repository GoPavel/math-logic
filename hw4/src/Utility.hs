{-# LANGUAGE UnicodeSyntax #-}

module Utility (    isNothing, isJust, fromJust, removeSpace, removeSpaces, insertIfAbsent,
                    insertIfAbsentIf, splitOn1, splitOn2, dropTail, eitherIfElse, fromRight,
                    fromLeft, third, showAll, showLines, showCom, times, times3, listSqr,
                    listCube) where

import           Data.List
import           Data.Map.Strict as Map (Map, insert, lookup)

isNothing ∷ Maybe a → Bool
isNothing (Just _) = False
isNothing Nothing  = True

isJust ∷ Maybe a → Bool
isJust = not . isNothing

fromJust ∷ Maybe a → a
fromJust (Just a) =  a
fromJust Nothing  = undefined

removeSpace ∷ String → String
removeSpace = foldr f "" where
    f c acc | c == ' ' || c == '\t' || c == '\r' = acc
            | otherwise = c : acc

removeSpaces ∷ [String] → [String]
removeSpaces = map removeSpace

insertIfAbsent ∷ Ord k => k → a → Map k a → Map k a
insertIfAbsent k a m = case Map.lookup k m of
    Just old → m
    Nothing  → Map.insert k a m

-- p (x , old) = 1 => x → map
-- p (x , old) = 0 => map
insertIfAbsentIf ∷ Ord k => (a → a → Bool) → k → a → Map k a → Map k a
insertIfAbsentIf p k a m = case Map.lookup k m of
    Nothing → Map.insert k a m
    Just old → if p a old
                then Map.insert k a m
                else m

splitOn2 ∷ (Char, Char) → String → [String]
splitOn2 _ [] = []
splitOn2 (a, b) xs = reverse $ func (a, b) xs [] [] where
    func ∷ (Char, Char) → String → String → [String] → [String]
    func (c, d) (a:b:xs) rs xss =
        if a == c && b == d
            then func (c, d) xs [] (reverse rs : xss)
            else func (c, d) (b : xs) (a : rs) xss
    func _ [a] rs xss = reverse (a : rs) : xss
    func _ [] rs xss = reverse rs : xss
-- O(N) :)

splitOn1 ∷ Char → String → [String]
splitOn1 _ [] = []
splitOn1 ch xs = reverse $ func ch xs [] [] where
    func ∷ Char → String → String → [String] → [String]
    func ch (x:xs) rs xss =
        if x == ch
            then func ch xs [] (reverse rs : xss)
            else func ch xs (x : rs) xss
    func _ [] rs xss = reverse rs : xss
-- O(N) :)

dropTail ∷ Int → [a] → [a]
dropTail i xs = reverse $ drop i (reverse xs)
-- O(N)

eitherIfElse ∷ Bool → a → b → Either b a
eitherIfElse flag r l = if flag then Right r else Left l

fromRight ∷ Either a b → b
fromRight (Right b) = b
fromRight (Left _)  = undefined

fromLeft ∷ Either a b → a
fromLeft (Left a)  = a
fromLeft (Right _) = undefined

third ∷ (a, b, c) → c
third (_, _, c) = c

showAll ∷ Show a => String → [a] → String
showAll str = (intercalate str) . (map show)

showLines ∷ Show a => [a] → String
showLines = showAll "\n"

showCom ∷ Show a => [a] → String
showCom = showAll ", "

times ∷ [a] → [b] → [(a, b)]
times as bs = foldl (\s a -> s ++ zip (repeat a) bs) [] as

times3 ∷ [a] → [b] → [c] → [(a, b, c)]
times3 as bs cs = foldl (\s a -> s ++ (map (\(b, c) ->  (a, b, c)) (bs `times` cs))) [] as

listSqr ∷ [a] → [(a, a)]
listSqr xs = times xs xs

listCube ∷ [a] → [(a, a, a)]
listCube xs = times3 xs xs xs
