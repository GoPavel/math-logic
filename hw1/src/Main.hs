module Main where

import Control.Monad
import System.IO
import Data.List
import Data.List.Split
import Grammar
import Lexer (alexScanTokens)
import Parser (parseExpr)

inputFile :: String
inputFile = "axioms.txt"

outputFile :: String
outputFile = "output.txt"

removeSpace :: String -> String
removeSpace str = foldr f "" str where
    f c acc | (c == ' ' || c == '\t' || c == '\r') = acc
            | otherwise = c : acc

removeSpaces :: [String] -> [String]
removeSpaces strs = map f strs where
    f' c acc | (c == ' ' || c == '\t' || c == '\r') = acc
             | otherwise = c : acc
    f str = foldr f' "" str

data Prop = Prop {
    index :: Int,
    strExpr :: String,
    expr :: Expr,
    indexOfAxiom :: Maybe Int,
    indexOfHypo :: Maybe Bool,
    mp :: Maybe (Int, Int) }

getAnnotate :: Prop -> String
getAnnotate prop = case indexOfAxiom prop of
    (Just i) -> "(Сх. акс. " ++ show i ++ ")"
    Nothing -> case indexOfHypo prop of
        (Just i) -> "(Предп. " ++ show i ++ ")"
        Nothing -> case mp prop of
            (Just (a, b)) -> "M. P. " ++ show a ++ ", " ++ show b ++ ")"
            Nothing -> "(Не доказано)"

instance Show Prop where
    show prop = "(" ++ (show $ index prop) ++ ") "  ++ strExpr prop ++ " " ++ getAnnotate prop


getExpr :: String -> Expr
getExpr s = case parseExpr (alexScanTokens s) of
    Left err -> error "error parse!!! in \"toProp\""
    Right expr -> expr

toProp :: (Int, String) -> Prop
toProp (i, s) = Prop {index = i,
                    strExpr = s,
                    expr = getExpr s,
                    indexOfAxiom = Nothing,
                    indexOfHypo = Nothing,
                    mp = Nothing}

toProps :: [String] -> [Prop]
toProps strs = map toProp (zip [1..] strs)

strBefore :: String -> String -> String
strBefore sep str = let (Just index) = findIndex (== '-') str
                    in take (index - 1) str

-- annotate :: [String] -> -> [String]
-- annotate hypos conclusion prots

isAxiom :: Expr -> Bool
isAxiom (Binary Impl a b) = True
isAxiom _ = False

-- annotateAxiom :: Prop -> Prop
-- annotateAxiom prop

main :: IO ()
main = do
    file <- readFile inputFile
    file <- return $ removeSpace file
    lineOfFiles <- return $ lines file
    headFile <- return $ splitOn "|-" (head lineOfFiles)
    lineOfFiles <- return $ drop 1 lineOfFiles
    hypos <- return $ splitOn "," (head headFile)
    conclusion <- return $ last headFile
    props <- return $ toProps lineOfFiles
    writeFile outputFile $ "\nHypos:\n" ++ show hypos
    appendFile outputFile $ "\nconclusion:\n" ++ conclusion
    appendFile outputFile $ "\nprops:\n" ++ intercalate "\n" (map show props)
    appendFile outputFile $ intercalate "\n" (map (show . getExpr) lineOfFiles)
    -- appendFile outputFile (intercalate "\n" annotate hypos conclusion prots)
--1:[2] 2:[] []
-- [1..60000] <=> init: 1, inc : (+ 1)
-- summ acc [] = acc
-- summ acc [x:xs] = summ (acc + x) xs

-- MonadReader : get
-- MonadState
-- StateT

-- axioms :: [Expr]
-- isAxiom :: Expr -> [Expr] -> (Expr, Bool)
-- result exprs = runState exprs solver
-- solver = do
--   exs <- get
--   put (map (isAxiom axioms)) exs
--   exs <- get
--   put (map (isHypo Hypos)) exs
--   exs <- get
--
--   return exs

-- data State = State {
--  hypos :: [Hypos]
--}


--  do ...
--  axiom <- getAxioms . get
--  hypos <- getHypos . get
--  state <- get
--  put (addHyposAnAxiomToMP axiom hypos state)
--  s <- get
--  put $ while goodState s changeState

-- while predicate init transformer =
-- if predicate init
--   then while predicate (transformer init) transformer
--   else init

-- changeState :: TypeState -> TypeState
