{-# LANGUAGE UnicodeSyntax   #-}

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
    index        ∷ Int,
    -- strExpr      ∷ String,
    getExpr      ∷ Expr,
    indexOfAxiom ∷ Maybe Int,
    indexOfHypo  ∷ Maybe Int,
    mp           ∷ Maybe (Int, Int) }
    deriving (Eq, Ord)

data Proof = Proof {
    getHypothesis ∷ [Expr],
    getConclusion ∷ Expr,
    getProps ∷ [Prop]
}

getMap ∷ [Prop] → Map.Map Int Expr
getMap props = Map.fromList (map (\prop → (index prop, getExpr prop)) props)

isAxiom ∷ Prop → Bool
isAxiom = isJust . indexOfAxiom

isHypos ∷ Prop → Bool
isHypos = isJust . indexOfHypo

isMP ∷ Prop → Bool
isMP = isJust . mp

instance Show Proof where
    show (Proof hypos concl props) =
        intercalate "," (map show hypos) ++ "|-" ++
        show concl ++ "\n" ++
        intercalate "\n" (map (show . getExpr) props)

nonAnnotated ∷ Int → Expr → Prop
nonAnnotated i e = Prop{
        index = i,
        getExpr = e,
        indexOfAxiom = Nothing,
        indexOfHypo = Nothing,
        mp = Nothing
    }

getAnnotate ∷ Prop → String
getAnnotate prop = case indexOfAxiom prop of
    (Just i) → "(Сх. акс. " ++ show i ++ ")"
    Nothing → case indexOfHypo prop of
        (Just i) → "(Предп. " ++ show i ++ ")"
        Nothing → case mp prop of
            (Just (a, b)) → "(M.P. " ++ show a ++ ", " ++ show b ++ ")"
            Nothing       → "(Не доказано)"

instance Show Prop where
    show prop = "(" ++ show (index prop) ++ ") "  ++ (show . getExpr) prop ++ " " ++ getAnnotate prop


exprFormString ∷ String → Expr
exprFormString s = case parseExpr (alexScanTokens s) of
    Left err   → error "error parse!!!"
    Right expr → expr
    
checkAxiom ∷ Expr → Maybe Int
checkAxiom (a1 :-> b :-> a1)
    | a1 == a2 =                                                Just 1
checkAxiom ((a1 :-> b1) :-> (a2 :-> b2 :-> c1) :-> (a3 :-> c2))
    | a1 == a2 && a2 == a3 && b1 == b2 && c1 == c2 =            Just 2
checkAxiom (a1 :-> b1 :-> a2 :& b2)
    | a1 == a2 && b1 == b2 =                                    Just 3
checkAxiom (a :& b :-> c)
    | a == c =                                                  Just 4
    | b == c =                                                  Just 5
checkAxiom (c :-> a :| b)
    | c == a =                                                  Just 6
    | c == b =                                                  Just 7
checkAxiom ((a1 :-> b1) :-> (c1 :-> b2) :-> (a2 :| c2 :-> b3))
    | a1 == a2 && c1 == c2 && b1 == b2 && b2 == b3 =            Just 8
checkAxiom ((a1 :-> b1) :-> (a2 :-> Not b2) :-> Not a3)
    | a1 == a2 && a2 == a3 && b1 == b2 =                        Just 9
checkAxiom (a1 :-> Not (Not a2))
    | a1 == a2 =                                                Just 10
checkAxiom _  =                                                 Nothing

annotateAxiom ∷ Prop → Prop
annotateAxiom prop = prop{indexOfAxiom = checkAxiom $ getExpr prop}

annotateHypos ∷ [Expr] → Prop → Prop
annotateHypos hypos prop = prop{indexOfHypo = Map.lookup (getExpr prop) mapOfHypos}
 where
     mapOfHypos =  Map.fromList (zip hypos ([1..]∷[Int]))

splitImpl ∷ Expr → Maybe (Expr, Expr)
splitImpl (a :-> b) = Just (a, b)
splitImpl _ = Nothing

oneIsAxiomOrHypo ∷ Prop → Bool
oneIsAxiomOrHypo Prop{ indexOfAxiom = a, indexOfHypo = b} = isJust a && isJust b

annotateMP ∷ [Prop] → [Prop]
annotateMP props = map (checkMP $ getMapMP props) props where
        -- A , A → B

        toExpr ∷ [Prop] → [Expr]
        toExpr = map getExpr

        getMapA ∷ [Prop] → Map.Map Expr Int
        getMapA props = Map.fromListWith min $ zip (toExpr props) [1..]

        getMapMP ∷ [Prop] → Map.Map Expr (Int, Int)
        getMapMP props = let
            step ∷ [Expr] → Int → Map.Map Expr Int → Map.Map Expr (Int, Int) → Map.Map Expr (Int, Int)
            step (x:xs) i mapA acc = case splitImpl x of
                Just (a, b) → case Map.lookup a mapA of
                                Nothing → step xs (i+1) mapA acc
                                Just j → step xs (i+1) mapA (
                                                            insertIfAbsentIf
                                                            (\(a, b) (c, d) → (max a b) < (max c d))
                                                            b (i, j) acc)
                Nothing → step xs (i+1) mapA acc
            step _ i mapA acc = acc
            in step (toExpr props) 1 (getMapA props) (Map.empty ∷ Map.Map Expr (Int, Int))

        checkMP ∷ Map.Map Expr (Int, Int) → Prop → Prop
        checkMP mapMP prop@Prop{getExpr = e, index = i} = prop {
            mp = case Map.lookup e mapMP of
                    Nothing → Nothing
                    p@(Just (j, k)) → if i > j && i > k then p else Nothing }

annotate ∷ ([Expr], Expr, [Expr]) → Maybe Proof
annotate (hypos, conclusion, props) = do
    props ← return $ (annotateMP . map (
        annotateHypos .
        annotateAxiom .
        uncarry nonAnnotated) .
        zip ([1..]::Int)) props
    return (Proof hypos conclusion props)

parseAndAnnotate ∷ String → Maybe Proof
parseAndAnnotate text = do
    text ← return $ removeSpace text
    lineOfFiles ← return $ filter (not . null) (lines text)
    headFile ← return $ splitOn2 ('|', '-') (head lineOfFiles)
    lineOfFiles ← return $ drop 1 lineOfFiles
    hypos ← return $ splitOn1 ',' (head headFile)
    conclusion ← return $ exprFormString $ last headFile
    hypos ← return $ if null hypos then [] else map exprFormString hypos
    props ← return $ map exprFormString lineOfFiles
    return annotate (hypos, conclusion, props)
