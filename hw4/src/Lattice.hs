{-# LANGUAGE UnicodeSyntax #-}

module Lattice(check, debug) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Utility

data Relation = L | G | N | E
    deriving (Show)

-- String{graph} → String{answer}
check ∷ Map.Map Int [Int] → String
check = undefined

debug ∷ Map.Map Int [Int] → String
debug g = showLines $ Map.toList $ getRelationTable g


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
isLess table (a, b) = case (a, b) Map.! table of
  L → True
  E → True
  _ → False

isGreater ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isGreater table (a, b) = case (a, b) Map.! table of
  G → True
  E → True
  _ → False

isEq ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isEq table (a, b) = (a, b) Map.! table == E

isNon ∷ Map.Map (Int, Int) Relation → (Int, Int) → Bool
isNon table (a, b) = (a, b) Map.! table == N


-- checkSum (listSqr vs) ab table
checkSum ∷ [(Int, Int)] → [Int] → Map.Map (Int, Int) Relation → Either (Map.Map (Int, Int) Int) String
checkSum pairs vs table = foldl checkPair (Right Map.empty) pairs
    where
        candidates ∷ (Int, Int) → [Int]
        candidates (a, b) =
            let predicate c = isGreater table (c, a) && isGreater table (c, b)
            in filter predicate vs

        minCandidate ∷ [Int] → Maybe Int
        minCandidate cs = foldl min' Nothing cs
            where
                min' ∷ Maybe Int → Int → Maybe Int
                min' m@(Just curMin) x = case (m, x) Map.! table of
                    L → Just m
                    G → Just m
                    E → Nothing
                    N → Nothing
                min' Nothing x = Just x

        checkPair ∷ Either (Map.Map (Int, Int) Int) String → (Int, Int) → Either (Map.Map (Int, Int) Int) String
        checkPair sums@(Left msg) _ = sums
        checkPair sums@(Right sumTable) pair@(a, b) =
            case minCandidate $ candidates pair of
                Nothing  → Left "Операция \'+\' не определена " ++ show a ++ "+" ++ show b
                (Just m) → sumTable `Map.insert` (pair, m)


checkTimes ∷ Map.Map Int [Int] → Map.Map (Int, Int) Relation → Either (Map.Map (Int, Int) Int) String
checkTimes = undefined

checkDistr ∷ Map.Map Int [Int] → Map.Map (Int, Int) Int
                               → Map.Map (Int, Int) Int → Either (Map.Map (Int, Int) Int) String
checkDistr = undefined

-- checkImpl ∷ Map.Map Int [Int] → Either String String
-- checkImpl graph = undefined
