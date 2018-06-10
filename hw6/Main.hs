{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           System.IO (readFile, writeFile)
import           Utility
import           Kripke (parseAndGetAnswer)

inputFile = "input.txt"
-- inputFile = "test1.txt"
-- inputFile = "test2.txt"
-- inputFile = "test3.txt"
-- inputFile = "test4.txt"
-- inputFile = "test5.txt"
-- inputFile = "test6.txt"
-- inputFile = "test7.txt"

outputFile = "output.txt"

main ∷ IO()
main = do
    text ← readFile inputFile
    writeFile outputFile (parseAndGetAnswer text)
