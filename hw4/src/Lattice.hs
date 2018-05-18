{-# LANGUAGE UnicodeSyntax #-}

module Lattice(check, debug) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Utility

data Relation = L | G | N | E
    deriving (Show, Eq)

sumFail (a, b) = "Операция \'+\' не определена " ++ show a ++ "+" ++ show b
timesFail (a, b) = "Операция \'*\' не определена " ++ show a ++ "*" ++ show b
distrFail (a, b, c) = undefined

-- String{graph} → String{answer}
check ∷ Map.Map Int [Int] → String
check = undefined

debug ∷ Map.Map Int [Int] → String
-- debug g = showLines $ Map.toList $ getRelationTable g
debug g =
    let table = getRelationTable g
    in case checkTimes g table of
        (Left str) → str
        (Right timesTable) → showLines $ Map.toList timesTable
-- debug g =
    -- let table = getRelationTable g
    -- in case checkSum g table of
    --     (Left str) → str
    --     (Right sumTable) → showLines $ Map.toList sumTable



getRelationTable ∷ Map.Map Int [Int] → Map.Map (Int, Int) Relation
getRelationTable graph =
    let vertices = Map.keys graph
        initTable = Map.fromList $ zip (listSqr vertices) (repeat N)
    in foldl accumulation initTable vertices
    where
        accumulation ∷ Map.Map (Int, Int) Relation → Int → Map.Map (Int, Int) Relation
        accumulation table v =
            let pairsLessV = zip (repeat v) (Set.toList $ dfs graph v Set.empty)
                pairsGreaterV = zip (Set.toList $ dfs graph v Set.empty) (repeat v)
                table' = foldl updateLess table pairsLessV
                table'' = foldl updateGreater table' pairsGreaterV
            in table''

        dfs ∷ Map.Map Int [Int] → Int → Set.Set Int → Set.Set Int
        dfs graph v used =
            if v `Set.member` used
                then used
                else foldl (\used v → dfs graph v used) (v `Set.insert` used) (graph Map.! v)

        updateGreater ∷ Map.Map (Int, Int) Relation → (Int, Int) → Map.Map (Int, Int) Relation
        updateGreater table pair = case table Map.! pair of
           L → Map.insert pair E table
           G → table
           N → Map.insert pair G table
           E → table

        updateLess ∷ Map.Map (Int, Int) Relation → (Int, Int) → Map.Map (Int, Int) Relation
        updateLess table pair = case table Map.! pair of
          L → table
          G → Map.insert pair E table
          N → Map.insert pair L table
          E → table

isLess ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isLess table pair = table Map.! pair == L

isGreater ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isGreater table pair = table Map.! pair == G

isEq ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isEq table pair = table Map.! pair == E

isNon ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isNon table pair = table Map.! pair == N

isGreaterEq ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isGreaterEq table pair = isGreater table pair || isEq table pair

isLessEq ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isLessEq table pair = isLess table pair || isEq table pair

-- checkSum (listSqr vs) ab table
checkSum ∷  Map.Map Int [Int] → Map.Map (Int, Int) Relation → Either String (Map.Map (Int, Int) Int)
checkSum graph table = foldl evalSumPair (Right Map.empty) pairs
    where
        vs ∷ [Int]
        vs = Map.keys graph

        pairs ∷ [(Int, Int)]
        pairs = listSqr vs

        candidates ∷ (Int, Int) → [Int]
        candidates (a, b) =
            let predicate c = isGreaterEq table (c, a) && isGreaterEq table (c, b)
            in filter predicate vs

        minCandidate ∷ [Int] → Maybe Int
        minCandidate cs = foldl min' Nothing cs
            where
                min' ∷ Maybe Int → Int → Maybe Int
                min' curMin@(Just m) x = case table Map.! (m, x) of
                    L → curMin
                    G → Just x
                    E → Nothing
                    N → Nothing
                min' Nothing x = Just x

        checkUniqueMin ∷ Int → [Int] → Bool
        checkUniqueMin m xs = all (\x → isGreater table (x, m) || x == m) xs

        evalSumPair ∷ Either String (Map.Map (Int, Int) Int) → (Int, Int) → Either String (Map.Map (Int, Int) Int)
        evalSumPair sums@(Left msg) _ = sums
        evalSumPair sums@(Right sumTable) pair =
            let mins = candidates pair
            in case minCandidate mins of
                Nothing  → Left $ sumFail pair
                -- (Just m) → if True
                (Just m) → if checkUniqueMin m mins
                    then Right $ Map.insert pair m sumTable
                    else Left $ sumFail pair

checkTimes ∷ Map.Map Int [Int] → Map.Map (Int, Int) Relation → Either String (Map.Map (Int, Int) Int)
checkTimes graph table = foldl evalTimesPair (Right Map.empty) pairs
    where
        vs ∷ [Int]
        vs = Map.keys graph

        pairs ∷ [(Int, Int)]
        pairs = listSqr vs

        candidates ∷ (Int, Int) → [Int]
        candidates (a, b) =
            let predicate x = isLessEq table (x, a) && isLessEq table (x, b)
            in filter predicate vs

        maxCandidate ∷ [Int] → Maybe Int
        maxCandidate cs = foldl max' Nothing cs
            where
                max' ∷ Maybe Int → Int → Maybe Int
                max' curMax@(Just m) x = case table Map.! (m, x) of
                    L → Just x
                    G → curMax
                    E → Nothing
                    N → Nothing
                max' Nothing x = Just x

        checkUniqueMax ∷ Int → [Int] → Bool
        checkUniqueMax m xs = all (\x -> isLess table (x, m) || x == m) xs

        evalTimesPair ∷ Either String (Map.Map (Int, Int) Int) → (Int, Int) → Either String (Map.Map (Int, Int) Int)
        evalTimesPair times@(Left msg) _ = times
        evalTimesPair times@(Right timesTable) pair =
            let maxs = candidates pair
            in case maxCandidate maxs of
                Nothing → Left $ timesFail pair
                (Just m) → if checkUniqueMax m maxs
                    then Right $ Map.insert pair m timesTable
                    else Left $ timesFail pair

checkDistr ∷ Map.Map Int [Int] → Map.Map (Int, Int) Int
                               → Map.Map (Int, Int) Int → Either String (Map.Map (Int, Int) Int)
checkDistr = undefined

-- checkImpl ∷ Map.Map Int [Int] → Either String String
-- checkImpl graph = undefined
