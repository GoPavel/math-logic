{-# LANGUAGE UnicodeSyntax #-}

module Kripke (kripkeToHeything, readKripke, checkExprInKripke,
               checkKripke, World(..), parseAndGetAnswer) where


import           Grammar
import           Utility (exprFormString, eitherIfElse, splitOn1)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Monad
import           Data.List
import           Data.Function

data World = World {
    worldVars ∷ [String],
    worldNexts ∷ [World] }

emptyWorld :: World
emptyWorld = World{worldVars = [], worldNexts = []}

data Heyting = Heyting {
    heytingGraph ∷ Map.Map Int [Int],
    heytingVars ∷ Map.Map String Int
}

instance Show Heyting where
    show Heyting{heytingGraph = g, heytingVars = vars} = showGraph g ++ showVars vars where
            showNode (node, list) = unwords (map show list)
            showGraph g = intercalate "\n" $ map showNode (Map.toAscList g)

            showVar (name, value) = name ++ "=" ++ show value
            showVars vs = intercalate "," $ map showVar (Map.toAscList vs)

parseAndGetAnswer ∷ String → String
parseAndGetAnswer text = case parseAndGetAnswer' text of
    (Right str) → str
    (Left str) → str
    where

    parseAndGetAnswer' ∷ String → Either String String
    parseAndGetAnswer' text = do
        let linesOfText = lines text
        let expr = exprFormString $ head linesOfText
        kripke <- readKripke $ drop 1 linesOfText
        eitherIfElse (checkKripke kripke) "Не модель Крипке" "Ok1"
        eitherIfElse (checkExprInKripke kripke expr)  "Не опровергает формулу" "Ok2"
        return $ show $ kripkeToHeything kripke

readKripke ∷ [String] → Either String World
readKripke strs = if length (worldNexts root) == 1
    then Right $ head $ worldNexts root
    else Left "Не дерево"
 where
    root = recur strs [emptyWorld] 0

    addSon ∷ World → World → World
    addSon p v = p{worldNexts = v : worldNexts p }

    recur ∷ [String] → [World] → Int → World
    recur [] [root] depth = root
    recur (str:strs) (v:stack) depth =
        let cntSpace = length $ takeWhile (/= '*') str
            varsFormString = splitOn1 ',' $ dropWhile (== ' ') $ drop (cntSpace + 1) str
            newWorld = emptyWorld{worldVars =  varsFormString}
            call'recur stack =  recur strs stack cntSpace
        in  if cntSpace > depth then call'recur (newWorld : addSon v newWorld : stack)
            else if cntSpace == depth then call'recur (addSon v newWorld : stack)
            else case drop (depth - cntSpace) (v : stack) of
                (v:vs) → call'recur (addSon v newWorld : vs)

checkKripke ∷ World → Bool
checkKripke root = checkKripke' root Set.empty  where
    checkKripke' ∷ World → Set.Set String → Bool
    checkKripke' World{worldNexts = nexts, worldVars = vars} varsOfparent = case nexts of
        [] → checkContainsAll vars varsOfparent
        _ → checkContainsAll vars varsOfparent && all (\world -> checkKripke' world $ varsOfparent `Set.union` Set.fromList vars)  nexts

    checkContainsAll ∷ [String] → Set.Set String → Bool
    checkContainsAll xs set = all (`Set.member` set) xs

checkExprInKripke ∷ World → Expr → Bool
checkExprInKripke root expr =
    checkExprInWorld root expr &&
    all (`checkExprInKripke` expr) (worldNexts root)

checkExprInWorld ∷ World → Expr → Bool
checkExprInWorld world expr = case expr of
    (a :& b) → on (&&) (checkExprInWorld world) a b
    (a :| b) → on (||) (checkExprInWorld world) a b
    (Not a) → notPred a world &&
                (null (worldNexts world) ||
                 all (notPred a) (worldNexts world))
    (a :-> b) → implPred a b world &&
                (null (worldNexts world) ||
                 all (implPred a b) (worldNexts world))
    (Var name) → name `Set.member` (Set.fromList $ worldVars world)

    where
        notPred expr = not . (`checkExprInWorld` expr)
        implPred a b = \x → not (checkExprInWorld x a) || checkExprInWorld x b

kripkeToHeything ∷ World → Heyting
kripkeToHeything world = undefined
