{-# LANGUAGE UnicodeSyntax #-}

module Annotation(annotate) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           ExprUtil        (checkAxiom)
import           Grammar
import           Proof
import           System.IO
import           Utility

annotateAxiom ∷ Prop → Prop
annotateAxiom prop = prop{indexOfAxiom = checkAxiom $ getExpr prop}

annotateHypos ∷ [Expr] → Prop → Prop
annotateHypos hypos prop = prop{indexOfHypo = Map.lookup (getExpr prop) mapOfHypos}
 where
     mapOfHypos =  Map.fromList (zip hypos ([1..]∷[Int]))

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
                    Nothing         → Nothing
                    p@(Just (j, k)) → if i > j && i > k then p else Nothing }

annotate ∷ ([Expr], Expr, [Expr]) → Proof
annotate (hypos, conclusion, exprs) =
    let props = (annotateMP .
                map (   annotateHypos .
                        annotateAxiom .
                        uncarry nonAnnotated)
                . zip ([1..]::Int)) exprs
    in Proof hypos conclusion props
