{-# LANGUAGE UnicodeSyntax     #-}

module Main where

-- import ProofGenerator (proofGen)
import System.IO
import Data.List
import Utility
import Grammar
import Parser (parseExpr)
import Lexer (alexScanTokens)

inputFile = "tests/test0.in"
outputFile = "output.txt"

main :: IO ()
main =  do
    text ← readFile inputFile
    writeFile outputFile ""
    answer ← proofGen text
    writeFile outputFile $ show (parseExpr (alexScanTokens text))
