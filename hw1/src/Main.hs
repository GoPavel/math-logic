module Main(main) where

import           Data.List
import           Control.Monad
-- import           Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Grammar
import           Lexer           (alexScanTokens)
import           Parser          (parseExpr)
import           System.IO
import           Utility

inputFile :: String
inputFile = "test9.txt"

outputFile :: String
outputFile = "output.txt"

data Prop = Prop {
    index        :: Int,
    -- strExpr      :: String,
    expr         :: Expr,
    indexOfAxiom :: Maybe Int,
    indexOfHypo  :: Maybe Int,
    mp           :: Maybe (Int, Int) }
    deriving (Eq, Ord)

getAnnotate :: Prop -> String
getAnnotate prop = case indexOfAxiom prop of
    (Just i) -> "(Сх. акс. " ++ show i ++ ")"
    Nothing -> case indexOfHypo prop of
        (Just i) -> "(Предп. " ++ show i ++ ")"
        Nothing -> case mp prop of
            (Just (a, b)) -> "(M.P. " ++ show a ++ ", " ++ show b ++ ")"
            Nothing       -> "(Не доказано)"

instance Show Prop where
    show prop = "(" ++ (show $ index prop) ++ ") "  ++ (show . expr) prop ++ " " ++ getAnnotate prop


getExpr :: String -> Expr
getExpr s = case parseExpr (alexScanTokens s) of
    Left err   -> error "error parse!!!"
    Right expr -> expr

toProp :: (Int, String) -> Prop
toProp (i, s) = Prop {index = i,
                    expr = getExpr s,
                    indexOfAxiom = Nothing,
                    indexOfHypo = Nothing,
                    mp = Nothing}

toProps :: [String] -> [Prop]
toProps strs = map toProp (zip [1..] strs)

strBefore :: String -> String -> String
strBefore sep str = let (Just index) = elemIndex '-' str
                    in take (index - 1) str

-- annotate :: [String] -> -> [String]
-- annotate hypos conclusion prots

isAxiom :: Expr -> Maybe Int
isAxiom (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Binary Impl b2 c1)) (Binary Impl a3 c2)))
    | a1 == a2 && a2 == a3 && b1 == b2 && c1 == c2 =            Just 2
    | otherwise =  Nothing
isAxiom (Binary Impl a1 (Binary Impl b1 (Binary And a2 b2)))
    | a1 == a2 && b1 == b2 =                                    Just 3
    | otherwise =  Nothing
isAxiom (Binary Impl (Binary And a b) c)
    | a == c =                                                  Just 4
    | b == c =                                                  Just 5
    | otherwise = Nothing
isAxiom (Binary Impl c (Binary Or a b))
    | c == a =                                                  Just 6
    | c == b =                                                  Just 7
    | otherwise =  Nothing
isAxiom (Binary Impl (Binary Impl a1 c1) (Binary Impl (Binary Impl b1 c2) (Binary Impl (Binary Or a2 b2) c3)))
    | a1 == a2 && c1 == c2 && c2 == c3 && b1 == b2 =            Just 8
    | otherwise =  Nothing
isAxiom (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Not b2)) (Not a3)))
    | a1 == a2 && a2 == a3 && b1 == b2 =                        Just 9
    | otherwise =  Nothing
isAxiom (Binary Impl (Not (Not a1)) a2)
    | a1 == a2 =                                                Just 10
    | otherwise =  Nothing
isAxiom (Binary Impl a1 (Binary Impl b1 a2))
    | a1 == a2 =                                                Just 1
    | otherwise =  Nothing
isAxiom _  = Nothing


annotateAxiom :: Prop -> Prop
annotateAxiom prop = prop{indexOfAxiom =  (isAxiom $ expr prop)}

annotateHypos :: Map.Map Expr Int -> Prop -> Prop
annotateHypos mapOfHypos prop = prop{indexOfHypo = (Map.lookup (expr prop) mapOfHypos)}

splitImpl :: Expr -> Maybe (Expr, Expr)
splitImpl (Binary Impl a b) = Just (a, b)
splitImpl _ = Nothing

oneIsAxiomOrHypo :: Prop -> Bool
oneIsAxiomOrHypo Prop{ indexOfAxiom = a, indexOfHypo = b} = isJust a && isJust b


annotateMP :: [Prop] -> [Prop]
annotateMP props = map (checkMP $ getMapMP props) props where
        -- A , A -> B, B
--
        toExpr :: [Prop] -> [Expr]
        toExpr = map expr
--
        getMapA :: [Prop] -> Map.Map Expr Int
        getMapA props = Map.fromListWith min $ zip (toExpr props) [1..]
--
        getMapMP :: [Prop] -> Map.Map Expr (Int, Int)
        getMapMP props = let
            step :: [Expr] -> Int -> Map.Map Expr Int -> Map.Map Expr (Int, Int) -> Map.Map Expr (Int, Int)
            step (x:xs) i mapA acc = case splitImpl x of
                Just (a, b) -> case Map.lookup a mapA of
                                Nothing -> step xs (i+1) mapA acc
                                Just j -> step xs (i+1) mapA (insertIfAbsent b (i, j) acc)
                Nothing -> step xs (i+1) mapA acc
            step _ i mapA acc = acc
            in step (toExpr props) 1 (getMapA props) (Map.empty :: Map.Map Expr (Int, Int))

        checkMP :: Map.Map Expr (Int, Int) -> Prop -> Prop
        checkMP mapMP prop@Prop{expr = e, index = i} = prop {
            mp = case Map.lookup e mapMP of
                    Nothing -> Nothing
                    p@(Just (j, k)) -> if i > j && i > k then p else Nothing }

main :: IO ()
main = do
    writeFile outputFile ""
    file <- readFile inputFile
    file <- return $ removeSpace file
    lineOfFiles <- return $ filter (not . null) (lines file)
    headFile <- return $ splitOn2 ('|', '-') (head lineOfFiles) --TODO
    lineOfFiles <- return $ drop 1 lineOfFiles
    hypos <- return $ splitOn1 ',' (head headFile)
    hypos <- return $ if null hypos then [] else map getExpr hypos
    hypos <- return $ Map.fromList (zip hypos ([1..]::[Int]))
    props <- return $ toProps lineOfFiles
    props <- return $ map annotateAxiom props
    props <- return $ map (annotateHypos hypos) props
    -- conclusion <- return $ getExpr $ last headFile
    -- alreadyExpr <- return $ map expr $ filter (\p -> isJust (indexOfAxiom p) && isJust (indexOfHypo p) ) props
    props <- return $ annotateMP props
    -- writeFile outputFile $ "\nHypos:\n" ++ show hypos
    -- appendFile outputFile $ "\nconclusion:\n" ++ conclusion
    -- appendFile outputFile $ "\nprops:\n" ++ intercalate "\n" (map show props)
    -- appendFile outputFile $ intercalate "\n" (map (show . getExpr) lineOfFiles)
    appendFile outputFile (intercalate "\n" (map show props))

-- 1:[2] 2:[] []
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
