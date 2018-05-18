{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Lattice         (check, debug)
import           System.IO       (readFile, writeFile)
import           Utility         (splitOn1)

inputFile = "input.txt"
-- inputFile =  "tests/one.txt"
-- inputFile =  "tests/cycle3.txt"
-- inputFile =  "tests/line2.txt"
-- inputFile =  "tests/line3.txt"
-- inputFile =  "tests/nonSum12.txt"
-- inputFile =  "tests/nonTimes34.txt"
-- inputFile =  "tests/ok4.txt"
-- inputFile =  "tests/nonBool9.txt"
-- inputFile =  "tests/nonDistr7.txt"
-- inputFile =  "tests/nonDistr.txt"


outputFile = "output.txt"

main ∷ IO()
main = do
    text ← readFile inputFile
    text ← return $ lines text
    text ← return $ filter (not . null) text
    let count = read (head text) ∷ Int
    text ← return $ drop 1 text
    text ← return $ map (filter (not . null)) $ map (splitOn1 ' ') text
    let graph = Map.fromList $ zip [1..] $ map (map (\x -> read x :: Int)) text
    writeFile outputFile ""
    -- writeFile outputFile $ "> "++ inputFile ++ "\n"
    -- appendFile outputFile $ Lattice.debug graph
    appendFile outputFile $ Lattice.check graph
    -- writeFile outputFile $ Map.showTree graph
