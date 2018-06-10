{-# LANGUAGE UnicodeSyntax #-}

module Kripke (kripkeToHeything, readKripke, checkExpr,
               checkKripke, World(..), parseAndGetAnswer) where


import           Grammar (exprFormString, eitherIfElse)
import           Utility
import qualified Data.Map.Strict as Map
import           Control.Monad
import           Data.List


data World = World {
    vars ∷ [String],
    nexts ∷ [World] }

emptyWorld :: World
emptyWorld = World{vars = [], nexts = []}

data Heyting = Heyting {
    graph ∷ Map.Map Int [Int],
    vars ∷ Map.Map String Int
}

instance Show Heyting where
    show Heyting {g = graph, vs = vars} = showGraph g ++ showVars where
            showNode (node, list) = intercalate " " (map show list)
            showGraph g = intercalate "\n" map showNode (Map.toAscList g)

            showVar (name, value) = name ++ "=" ++ value
            showVars vs = intercalate "," $ map showVar (Set.toAscList vs)

parseAndGetAnswer ∷ String → String
parseAndGetAnswer = case parseAndGetAnswer' of
    (Right str) → str
    (Left str) → str

parseAndGetAnswer' ∷ String → Either String String
parseAndGetAnswer' text = do
    let linesOfText = lines text
    let expr = exprFormString $ head linesOfText
    kripke <- readKripke $ drop 1 linesOfText
    eitherIfElse (checkKripke kripke) "Не модель Крипке" "Ok1"
    eitherIfElse (checkExpr kripke expr)  "Не опровергает формулу" "Ok2"
    return $ show $ kripkeToHeything kripke

readKripke ∷ [String] → Either String World
readKripke strs = if length (nexts root) == 1
    then Right $ head $ nexts root
    else Left "Не дерево"
 where
    root = recur strs emptyWorld 0

    addSon ∷ World → World → World
    addSon p v = p{nexts = v : (nexts p) }

    recur ∷ [String] → [Wolrd] → Int → World
    recur [] [root] depth = root
    recur (str:strs) (v:stack) depth =
        let cntSpace = length $ takeWhile (/= '*') str
            varsFormString = splitOn1 ',' $ dropWhile (== ' ') $ drop (cntSpace + 1) str
            newWorld = emptyWorld{vars =  varsFormString}
            call'recur stack =  recur strs stack cntSpace
        in  if cntSpace > depth then call'recur (newWorld : addSon v newWorld : stack)
            else if cntSpace == depth then call'recur (addSon v newWorld : stack)
            else case drop (depth - cntSpace) (v : stack) of
                (v:vs) → call'recur (addSon v newWorld : vs)

checkKripke ∷ World → Bool
checkKripke = undefined

kripkeToHeything ∷ World → Heyting
kripkeToHeything = undefined
