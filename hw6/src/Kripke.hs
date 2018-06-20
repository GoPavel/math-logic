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
    worldIndex ∷ Int,
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
readKripke strs = Kripke{kripkeRoots = worldNexts $ recur 0 strs [emptyWorld] 0}
 where
    addSon ∷ World → World → World
    addSon p v = p{worldNexts = v : worldNexts p }

    recur ∷ Int → [String] → [World] → Int → World
    recur index [] [root] depth = root
    recur index (str:strs) (v:stack) depth =
        let cntSpace = length $ takeWhile (/= '*') str
            varsFormString = splitOn1 ',' $ dropWhile (== ' ') $ drop (cntSpace + 1) str
            newWorld = emptyWorld{worldVars = Set.fromList varsFormString, worldIndex = index}
            call'recur stack =  recur (index + 1) strs stack cntSpace
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
kripkeToHeything world = undefined

    where
        getBaseSet ∷ World → Set.Set Int → Set.Set Int
        getBaseSet World{worldNexts = sons, worldIndex = index} set =
            foldr getBaseSet (index `Set.insert` set) sons

        getAllBaseSet ∷ Kripke → [Set.Set Int]
        getAllBaseSet Kripke{kripkeRoots = roots} = concatMap (`dfs` []) roots where
            dfs ∷ World → [Set.Set Int] → [Set.Set Int]
            dfs world list = foldr dfs (getBaseSet world Set.empty : list) (worldNexts world)


        getAllOpenSet ∷ Kripke → [Set.Set Int]
        getAllOpenSet kripke = recur (getAllBaseSet kripke) [] where
            recur ∷ [Set.Set Int] → [Set.Set Int] → [Set.Set Int]
            recur (set : sets) acc =
                recur sets (set : acc) ++ recur sets acc
            recur [] acc = [foldr Set.union Set.empty acc]

        makeGraph ∷ Kripke → Map.Map Int [Int]
        makeGraph kripke = Map.fromAscList $ map (\v -> (v, getConnection v)) (Map.keys sets) where
            openSets = getAllOpenSet kripke
            sets = Map.fromAscList $ zip [1..] openSets
            getConnection ∷ Int → [Int]
            getConnection v = filter (`isParent` v) (Map.keys sets)

            isParent ∷ Int -> Int -> Bool
            isParent v u = (sets Map.! v) `Set.isSubsetOf` (sets Map.! u)

 -- TODO set Var
 --     TODO save Var
 --     TODO find max
