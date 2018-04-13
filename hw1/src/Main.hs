module Main where

import           Data.List
import           Control.Monad
import           Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Grammar
import           Lexer           (alexScanTokens)
import           Parser          (parseExpr)
import           System.IO

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
    index        :: Int,
    strExpr      :: String,
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
            (Just (a, b)) -> "M. P. " ++ show a ++ ", " ++ show b ++ ")"
            Nothing       -> "(Не доказано)"

instance Show Prop where
    show prop = "(" ++ (show $ index prop) ++ ") "  ++ strExpr prop ++ " " ++ getAnnotate prop


getExpr :: String -> Expr
getExpr s = case parseExpr (alexScanTokens s) of
    Left err   -> error "error parse!!! in \"toProp\""
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
annotateAxiom prop@(Prop i s e ia ih ismp) = (Prop i s e (isAxiom $ expr prop) ih ismp)

annotateHypos :: Map.Map Expr Int -> Prop -> Prop
annotateHypos mapOfHypos prop@(Prop i s e ia ih ismp) = Prop i s e ia (Map.lookup e mapOfHypos) ismp

splitImpl :: Expr -> Maybe (Expr, Expr)
splitImpl (Binary Impl a b) = Just (a, b)
splitImpl _ = Nothing

isNothing :: Maybe a -> Bool
isNothing (Just a) = False
isNothing Nothing = True

isJust :: Maybe a -> Bool
isJust = not . isNothing

formJust :: Maybe a -> a
formJust (Just a) =  a
fromJust Nothing = undefined

fromListToMultiMap :: [(Expr, Expr)] -> Map.Map Expr (Set.Set Expr)
fromListToMultiMap dexprs = add (Map.empty :: Map.Map Expr (Set.Set Expr)) dexprs
    where
        add :: Map.Map Expr (Set.Set Expr) -> [(Expr, Expr)] -> Map.Map Expr (Set.Set Expr)
        add mmap ((e1, e2):dexprs) = add
                                    (
                                     if isNothing (Map.lookup e1 mmap)
                                     then Map.insert e1 (Set.singleton e2) mmap
                                     else Map.insert e1 (Set.insert e2 (fromJust $ Map.lookup e1 mmap))  mmap
                                     )
                                     dexprs
        add mmap _ = mmap


oneIsAxiomOrHypo :: Prop -> Bool
oneIsAxiomOrHypo Prop{ a = indexOfAxiom, b = indexOfHypo} = isJust a && isJust b

annotateMP :: Prop -> Map.Map Expr (Set.Set Expr) -> (Prop, Map.Map Expr (Set.Set Expr))
annotateMP prop mmap =
    if isNothing (Map.lookup (expr prop) mmap)
    then (prop, mmap)

main :: IO ()
main = do
    file <- readFile inputFile
    file <- return $ removeSpace file
    lineOfFiles <- return $ lines file
    headFile <- return $ splitOn "|-" (head lineOfFiles)
    lineOfFiles <- return $ drop 1 lineOfFiles
    hypos <- return $ splitOn "," (head headFile)
    hypos <- return $ map getExpr hypos
    hypos <- return $ Map.fromList (zip hypos ([1..]::[Int]))
    props <- return $ toProps lineOfFiles
    props <- return $ map annotateAxiom props
    props <- return $ map (annotateHypos hypos) props
    -- conclusion <- return $ getExpr $ last headFile
    alreadyExpr <- return $ map expr $ filter (\p -> isJust (indexOfAxiom p) && isJust (indexOfHypo p) ) props
    setProved <- return $ Set.fromList alreadyExpr
    mapBimplA <- return $ fromListToMultiMap $ map fromJust $ filter isJust $ map splitImpl alreadyExpr
    writeFile outputFile ""
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
