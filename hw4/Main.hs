{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import qualified Lattice (check)
import           System.IO (readFile, writeFile)
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import           Utility (splitOn1)

inputFile = "input.txt"
outputFile = "output.txt"

main ∷ IO()
main = do
    text ← readFile inputFile
    text ← return $ lines text
    let count = read (head text) ∷ Int
    text ← return $ drop 1 text
    text ← return $ map (filter (not . null)) $ map (splitOn1 ' ') text
    let graph = Map.fromList $ zip [1..] $ map (map (\x -> read x :: Int)) text
    writeFile outputFile ""
    writeFile outputFile $ Lattice.check graph
    -- writeFile outputFile $ Map.showTree graph
