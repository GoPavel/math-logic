module Main where

import System.IO
import Data.List
import Annotation
import Utility
import Grammar
import Deduction

inputFile = "input.txt"
outputFile = "output.txt"

main :: IO ()
main = do
    writeFile outputFile ""
    text <- readFile inputFile
    proof <- return $ fromJust $ parseAndAnnotate text
    -- appendFile outputFile (show proof)
    appendFile outputFile (show (applyDeduction proof))
