module Annotation where

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

data Prop = Prop {
    index        :: Int,
    -- strExpr      :: String,
    expr         :: Expr,
    indexOfAxiom :: Maybe Int,
    indexOfHypo  :: Maybe Int,
    mp           :: Maybe (Int, Int) }
    deriving (Eq, Ord)

data Proof = Proof {
    getHypothesis :: [Expr],
    getConclusion :: Expr,
    getProps :: [Prop]
}

getMap :: [Prop] -> Map.Map Int Expr
getMap props = Map.fromList (map (\prop -> (index prop, expr prop)) props) 

isAxiom = isJust . indexOfAxiom
isHypos = isJust . indexOfHypo
isMP = isJust . mp

showWithAnnotation (Proof hypos concl props) =
    intercalate "," (map show hypos) ++ "|-" ++
    show concl ++ "\n" ++
    intercalate "\n" (map show props)

instance Show Proof where
    show (Proof hypos concl props) =
        intercalate "," (map show hypos) ++ "|-" ++
        show concl ++ "\n" ++
        intercalate "\n" (map (show . expr) props)

getAnnotate :: Prop -> String
getAnnotate prop = case indexOfAxiom prop of
    (Just i) -> "(Сх. акс. " ++ show i ++ ")"
    Nothing -> case indexOfHypo prop of
        (Just i) -> "(Предп. " ++ show i ++ ")"
        Nothing -> case mp prop of
            (Just (a, b)) -> "(M.P. " ++ show a ++ ", " ++ show b ++ ")"
            Nothing       -> "(Не доказано)"

instance Show Prop where
    show prop = "(" ++ show (index prop) ++ ") "  ++ (show . expr) prop ++ " " ++ getAnnotate prop


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

checkAxiom :: Expr -> Maybe Int
checkAxiom (Binary Impl a1 (Binary Impl b1 a2))
    | a1 == a2 =                                                Just 1
checkAxiom (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Binary Impl b2 c1)) (Binary Impl a3 c2)))
    | a1 == a2 && a2 == a3 && b1 == b2 && c1 == c2 =            Just 2
checkAxiom (Binary Impl a1 (Binary Impl b1 (Binary And a2 b2)))
    | a1 == a2 && b1 == b2 =                                    Just 3
checkAxiom (Binary Impl (Binary And a b) c)
    | a == c =                                                  Just 4
    | b == c =                                                  Just 5
checkAxiom (Binary Impl c (Binary Or a b))
    | c == a =                                                  Just 6
    | c == b =                                                  Just 7
checkAxiom (Binary Impl (Binary Impl a1 c1) (Binary Impl (Binary Impl b1 c2) (Binary Impl (Binary Or a2 b2) c3)))
    | a1 == a2 && c1 == c2 && c2 == c3 && b1 == b2 =            Just 8
checkAxiom (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Not b2)) (Not a3)))
    | a1 == a2 && a2 == a3 && b1 == b2 =                        Just 9
checkAxiom (Binary Impl (Not (Not a1)) a2)
    | a1 == a2 =                                                Just 10
checkAxiom _  =                                                    Nothing




annotateAxiom :: Prop -> Prop
annotateAxiom prop = prop{indexOfAxiom =  (checkAxiom $ expr prop)}

annotateHypos :: [Expr] -> Prop -> Prop
annotateHypos hypos prop = prop{indexOfHypo = Map.lookup (expr prop) mapOfHypos}
 where
     mapOfHypos =  Map.fromList (zip hypos ([1..]::[Int]))

splitImpl :: Expr -> Maybe (Expr, Expr)
splitImpl (Binary Impl a b) = Just (a, b)
splitImpl _ = Nothing

oneIsAxiomOrHypo :: Prop -> Bool
oneIsAxiomOrHypo Prop{ indexOfAxiom = a, indexOfHypo = b} = isJust a && isJust b

annotateMP :: [Prop] -> [Prop]
annotateMP props = map (checkMP $ getMapMP props) props where
        -- A , A -> B

        toExpr :: [Prop] -> [Expr]
        toExpr = map expr

        getMapA :: [Prop] -> Map.Map Expr Int
        getMapA props = Map.fromListWith min $ zip (toExpr props) [1..]

        getMapMP :: [Prop] -> Map.Map Expr (Int, Int)
        getMapMP props = let
            step :: [Expr] -> Int -> Map.Map Expr Int -> Map.Map Expr (Int, Int) -> Map.Map Expr (Int, Int)
            step (x:xs) i mapA acc = case splitImpl x of
                Just (a, b) -> case Map.lookup a mapA of
                                Nothing -> step xs (i+1) mapA acc
                                Just j -> step xs (i+1) mapA (
                                                            -- insertIfAbsent
                                                            insertIfAbsentIf
                                                            (\(a, b) (c, d) -> (max a b) < (max c d))
                                                            b
                                                            (i, j)
                                                            acc)
                Nothing -> step xs (i+1) mapA acc
            step _ i mapA acc = acc
            in step (toExpr props) 1 (getMapA props) (Map.empty :: Map.Map Expr (Int, Int))

        checkMP :: Map.Map Expr (Int, Int) -> Prop -> Prop
        checkMP mapMP prop@Prop{expr = e, index = i} = prop {
            mp = case Map.lookup e mapMP of
                    Nothing -> Nothing
                    p@(Just (j, k)) -> if i > j && i > k then p else Nothing }

parseAndAnnotate :: String -> Maybe Proof
parseAndAnnotate text = do
    text <- return $ removeSpace text
    lineOfFiles <- return $ filter (not . null) (lines text)
    headFile <- return $ splitOn2 ('|', '-') (head lineOfFiles)
    lineOfFiles <- return $ drop 1 lineOfFiles
    hypos <- return $ splitOn1 ',' (head headFile)
    conclusion <- return $ getExpr $ last headFile
    hypos <- return $ if null hypos then [] else map getExpr hypos
    props <- return $ toProps lineOfFiles
    props <- return $ map annotateAxiom props
    props <- return $ map (annotateHypos hypos) props
    props <- return $ annotateMP props
    return (Proof hypos conclusion props)

-- hw1 :: String -> String -> IO ()
-- hw1 inputFile outputFile = do
--     writeFile outputFile ""
--     file <- readFile inputFile
--     file <- return $ removeSpace file
--     lineOfFiles <- return $ filter (not . null) (lines file)
--     headFile <- return $ splitOn2 ('|', '-') (head lineOfFiles) --TODO
--     lineOfFiles <- return $ drop 1 lineOfFiles
--     hypos <- return $ splitOn1 ',' (head headFile)
--     hypos <- return $ if null hypos then [] else map getExpr hypos
--     hypos <- return $ Map.fromList (zip hypos ([1..]::[Int]))
--     props <- return $ toProps lineOfFiles
--     props <- return $ map annotateAxiom props
--     props <- return $ map (annotateHypos hypos) props
--     props <- return $ annotateMP props
--     appendFile outputFile (intercalate "\n" (map show props))
