module Deduction where

import Annotation
import Grammar
import Utility
import qualified Data.Map.Strict as Map

aToA ∷ Expr → [Expr]

applyDeduction :: Proof -> Proof
applyDeduction (Proof hypos conclusion proofs) = (Proof trHypos trConclusion trProofs) where
    alpha = last hypos

    addAlpha e = alpha :-> e

    axiom1 a b = a :-> (b :-> a)

    axiom2 a b c = (a :-> b) :-> (a :-> b :-> c) :-> (a :-> c)

    alphaToAlpha e = (shc5 e : shc4 e : shc3 e : shc2 e : [shc1 e])  where
        shc1 e = e :-> e :-> e
        shc2 e = (e :-> (e :-> e)) :-> (e :-> ((e :-> e) :-> e)) :-> (e :-> e)
        shc3 e = (e :-> ((e :-> e) :-> e)) :-> (e :-> e)
        shc4 e = (e :-> ((e :-> e) :-> e))
        shc5 e = e :-> e

    getProofMP :: Prop -> Map.Map Int Expr -> Maybe [Expr]
    getProofMP Prop{expr = e, mp = Just (i, j)} m = do
        (dj, di) <- splitImpl $ fromJust $ Map.lookup i m
        return (alpha :-> di : ((alpha :-> (dj :-> di)) :-> (alpha :-> di)) : [axiom2 alpha dj di])
    getProofMP _ _ = Nothing


    trHypos :: [Expr]
    trHypos = dropTail 1 hypos

    trConclusion :: Expr
    trConclusion = addAlpha conclusion

    nonAnnotated' = nonAnnotated 0

    trProofs :: [Prop]
    trProofs = step proofs (getMap proofs) [] where
        step :: [Prop] -> Map.Map Int Expr -> [Expr] -> [Prop]
        step (p@Prop{expr = e}:ps) m stack
            | e == alpha = step ps m (alphaToAlpha e ++ stack)
            | isAxiom p || isHypos p = step ps m ( alpha :-> e : e : axiom1 e alpha : stack )
            | isMP p = step ps m $ (fromJust $ getProofMP p m) ++ stack
            | otherwise = error "Не доказательство"
        step [] _ stack = map nonAnnotated' (reverse stack)
