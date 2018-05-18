{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import qualified Lattice (check, debug)
import           System.IO (readFile, writeFile)
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import           Utility (splitOn1)

-- inputFile = "input.txt"
-- inputFile =  "tests/one.txt"
-- inputFile =  "tests/cycle3.txt"
-- inputFile =  "tests/line3.txt"
-- inputFile =  "tests/nonSum12.txt"
-- inputFile =  "tests/nonTimes34.txt"
-- inputFile =  "tests/ok4.txt"
-- inputFile =  "tests/ok7.txt"


outputFile = "output.txt"

main ∷ IO()
main = do
    text ← readFile inputFile
    text ← return $ lines text
    let count = read (head text) ∷ Int
    text ← return $ drop 1 text
    text ← return $ map (filter (not . null)) $ map (splitOn1 ' ') text
    let graph = Map.fromList $ zip [1..] $ map (map (\x -> read x :: Int)) text
    writeFile outputFile $ "> "++ inputFile ++ "\n"
    appendFile outputFile $ Lattice.debug graph
    -- writeFile outputFile $ Lattice.check graph
    -- writeFile outputFile $ Map.showTree graph
