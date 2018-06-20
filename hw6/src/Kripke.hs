{-# LANGUAGE UnicodeSyntax #-}

module Kripke (kripkeToHeything, readKripke, checkExprInKripke,
               checkKripke, World(..), parseAndGetAnswer) where

import           Grammar
import           Utility (exprFormString, eitherIfElse, splitOn1, fromRight)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Monad
import           Data.List
import           Data.Function

data World = World {
    worldVars ∷ Set.Set String,
    worldNexts ∷ [World]
}

data Kripke = Kripke {
    kripkeRoots :: [World]
}

emptyWorld ∷ World
emptyWorld = World{worldVars = Set.empty, worldNexts = []}

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
        kripke <- Right $ readKripke $ drop 1 linesOfText
        eitherIfElse (checkKripke kripke) "Не модель Крипке" "Ok1"
        eitherIfElse (checkExprInKripke kripke expr)  "Не опровергает формулу" "Ok2"
        return $ show $ kripkeToHeything kripke

readKripke ∷ [String] → Kripke
readKripke strs = Kripke{kripkeRoots = worldNexts $ recur strs [emptyWorld] 0}
 where
    addSon ∷ World → World → World
    addSon p v = p{worldNexts = v : worldNexts p }

    recur ∷ [String] → [World] → Int → World
    recur [] [root] depth = root
    recur (str:strs) (v:stack) depth =
        let cntSpace = length $ takeWhile (/= '*') str
            varsFormString = splitOn1 ',' $ dropWhile (== ' ') $ drop (cntSpace + 1) str
            newWorld = emptyWorld{worldVars = Set.fromList varsFormString}
            call'recur stack =  recur strs stack cntSpace
        in  if cntSpace > depth then call'recur (newWorld : addSon v newWorld : stack)
            else if cntSpace == depth then call'recur (addSon v newWorld : stack)
            else case drop (depth - cntSpace) (v : stack) of
                (v:vs) → call'recur (addSon v newWorld : vs)

checkKripke ∷ Kripke → Bool
checkKripke kripke = all (`checkKripke'` Set.empty) (kripkeRoots kripke)  where
    checkKripke' ∷ World → Set.Set String → Bool
    checkKripke' World{worldNexts = nexts, worldVars = vars} varsOfparent =
            checkContainsAll vars varsOfparent
        && (null nexts || all (\world -> checkKripke' world $ varsOfparent `Set.union` vars)  nexts)

    checkContainsAll ∷ Set.Set String → Set.Set String → Bool
    checkContainsAll xs set = all (`Set.member` set) xs

checkExprInKripke ∷ Kripke → Expr → Bool
checkExprInKripke kripke expr = all (`rec'checkExprInKripke` expr) (kripkeRoots kripke)
    where
        rec'checkExprInKripke ∷ World → Expr → Bool
        rec'checkExprInKripke root expr = root `checkExprInWorld` expr &&
            all (`rec'checkExprInKripke` expr) (worldNexts root)

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
    (Var name) → name `Set.member` worldVars world

    where
        notPred expr = not . (`checkExprInWorld` expr)
        implPred a b = \x → not (checkExprInWorld x a) || checkExprInWorld x b

kripkeToHeything ∷ Kripke → Heyting -- TODO
kripkeToHeything world = undefined -- where
--
--     all = map getSubTree root
--
--     getelements
