{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Annotation     (annotate, parseAndAnnotate)
import           Data.List
import           Grammar
import           ProofGenerator (proofGen)
import           System.IO
import           Utility

-- inputFile = "input.txt"
inputFile = "tests/test2.in"
outputFile = "output.txt"
-- checkputFile = "checkput.txt"

main :: IO ()
main =  do
    text ← readFile inputFile
    writeFile outputFile ""
    -- answer ← return $ debug
    answer ← return $ either id id (proofGen text)
    writeFile outputFile answer
    -- writeFile checkputFile (show $ fromJust $ parseAndAnnotate answer)
