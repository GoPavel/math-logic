{-# LANGUAGE UnicodeSyntax #-}

module Deduction(deduce) where

import           Annotation      (annotate)
import qualified Data.Map.Strict as Map
import           ExprUtil (proofAToA)
import           Grammar
import           Proof
import           Utility

deduce ∷ ([Expr], Expr, [Expr]) → ([Expr], Expr, [Expr])
deduce = deduceWithAnnotation . annotate

deduceWithAnnotation ∷ Proof → ([Expr], Expr, [Expr])
applyDeduction (Proof hypos conclusion proofs) = (trHypos, trConclusion, trProofs) where
    alpha = last hypos

    axiom2 a b c = (a :-> b) :-> (a :-> b :-> c) :-> (a :-> c)

    getProofMP ∷ Prop → Map.Map Int Expr → Maybe [Expr]
    getProofMP Prop{expr = e, mp = Just (i, j)} m = do
        (dj, di) <- splitImpl $ fromJust $ Map.lookup i m
        return  (   alpha :-> di
                :   ((alpha :-> (dj :-> di)) :-> (alpha :-> di))
                :   [axiom2 alpha dj di]
                )
    getProofMP _ _ = Nothing


    trHypos ∷ [Expr]
    trHypos = dropTail 1 hypos

    trConclusion ∷ Expr
    trConclusion = aplha :-> conclusion

    trProofs ∷ [Expr]
    trProofs = step proofs (getMap proofs) [] where
        step ∷ [Prop] → Map.Map Int Expr → [Expr] → [Expr]
        step (p@Prop{expr = e}:ps) m stack
            | e == alpha = step ps m (proofAToA e ++ stack)
            | isAxiom p || isHypos p = step ps m (  alpha :-> e
                                                 :  e
                                                 :  e :-> alpha :-> e
                                                 :  stack
                                                 )
            | isMP p = step ps m $ fromJust (getProofMP p m) ++ stack
            | otherwise = error "Не доказательство"
        step [] _ stack = reverse stack

        getMap ∷ [Prop] → Map.Map Int Expr
        getMap props = Map.fromList (map (\prop → (index prop, getExpr prop)) props)
