module Main where

import Control.Monad
import System.IO
import Data.List
import Data.List.Split
import Grammar
import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.Map.Strict (Map, insert, insertWith, empty, fromList, lookup)

inputFile :: String
inputFile = "test1.txt"

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
    indexOfHypo :: Maybe Int,
    mp :: Maybe (Int, Int) }
    deriving (Eq, Ord)

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

isAxiom :: Expr -> Maybe Int
isAxiom (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Binary Impl b2 c1)) (Binary Impl a3 c2))) = if a1 == a2 && a2 == a3 && b1 == b2 && c1 == c2 then (Just 2) else Nothing
isAxiom (Binary Impl a1 (Binary Impl b1 (Binary And a2 b2))) = if a1 == a2 && b1 == b2 then (Just 3) else Nothing
isAxiom (Binary Impl (Binary And a b) c) =
    if a == c then (Just 4)
    else if b == c then (Just 5)
    else Nothing
isAxiom (Binary Impl c (Binary Or a b)) =
    if c == a then (Just 6)
    else if c == b then (Just 7)
    else Nothing
isAxiom (Binary Impl (Binary Impl a1 c1) (Binary Impl (Binary Impl b1 c2) (Binary Impl (Binary Or a2 b2) c3))) = if a1 == a2 && c1 == c2 && c2 == c3 && b1 == b2 then (Just 8) else Nothing
isAxiom (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Not b2)) (Not a3))) = if a1 == a2 && a2 == a3 && b1 == b2 then (Just 9) else Nothing
isAxiom (Binary Impl (Not (Not a1)) a2) = if a1 == a2 then (Just 10) else Nothing
isAxiom (Binary Impl a1 (Binary Impl b1 a2)) = if a1 == a2 then (Just 1) else Nothing
isAxiom _  = Nothing


annotateAxiom :: Prop -> Prop
annotateAxiom prop@(Prop i s e ia ih ismp) = (Prop i s e (isAxiom $ expr prop) ih ismp)

annotateHypos :: Map Expr Int -> Prop -> Prop
annotateHypos mapOfHypos prop@(Prop i s e ia ih ismp) = (Prop i s e ia (Data.Map.Strict.lookup e mapOfHypos) ismp)

main :: IO ()
main = do
    file <- readFile inputFile
    file <- return $ removeSpace file
    lineOfFiles <- return $ lines file
    headFile <- return $ splitOn "|-" (head lineOfFiles)
    lineOfFiles <- return $ drop 1 lineOfFiles
    hypos <- return $ splitOn "," (head headFile)
    hypos <- return $ map getExpr hypos
    hypos <- return $ fromList (zip hypos ([1..]::[Int]))
    conclusion <- return $ last headFile
    props <- return $ toProps lineOfFiles
    props <- return $ map annotateAxiom props
    props <- return $ map (annotateHypos hypos) props
    writeFile outputFile $ ""
    -- writeFile outputFile $ "\nHypos:\n" ++ show hypos
    -- appendFile outputFile $ "\nconclusion:\n" ++ conclusion
    -- appendFile outputFile $ "\nprops:\n" ++ intercalate "\n" (map show props)
    -- appendFile outputFile $ intercalate "\n" (map (show . getExpr) lineOfFiles)
    appendFile outputFile (intercalate "\n" (map show props))
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
