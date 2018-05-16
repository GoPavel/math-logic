{-# LANGUAGE UnicodeSyntax #-}

module Lattice(check) where

import           Control.Monad
import           Utility
import           Data.List
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

data Relation = L | G | N | E

-- String{graph} → String{answer}
check ∷ Map.Map Int [Int] → String
check = undefined

getRelationTable ∷ Map.Map Int [Int] → Map.Map (Int, Int) Relation
getRelationTable graph = getTable graph (Map.keys graph)

    where
        getTable ∷ Map.Map Int [Int] → [Int] → Map.Map (Int, Int) Relation
        getTable graph (v:vs) table =
            let pairs = zip (repeat v) (dfs graph v Set.empty)
            in getTable graph vs (recur table pairs)

        recur ∷ Map.Map Int [Int] → [(Int, Int)] → Map.Map Int [Int]
        recur table (x:xs) = recur (adder table x) xs
        recur table [] = table

        dfs ∷ Map.Map Int [Int] → Int → Set.Set Int → [Int]
        dfs graph v used | Set.member v used = v ++
            concat $ map (\x -> dfs graph x (insert v used)) (graph !! v)
                         | otherwise = []

        adder ∷ Map.Map (Int, Int) Relation → (Int, Int) → Map.Map (Int, Int) Relation
        adder table pair = case table ! pair of
           L → insert pair E table
           G → table
           N → insert pair G table
           E → table





checkImpl ∷ Map.Map Int [Int] → Either String String
checkImpl graph = undefined
