module Main where

import Annotation

inputFile :: String
inputFile = "input.txt"

outputFile :: String
outputFile = "output.txt"

main :: IO ()
main = parseAndAnnotate inputFile outputFile
