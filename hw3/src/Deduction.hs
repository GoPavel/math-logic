{-# LANGUAGE UnicodeSyntax #-}

module Deduction(deduce, antiDeduce, contraposition, proofAOrNotA) where

import           Annotation      (annotate, proofAToA, isHypos, isAxiom, isMP)
import qualified Data.Map.Strict as Map
import           Grammar
import           Proof
import           Utility
import           Data.List

deduce ∷ ([Expr], Expr, [Expr]) → ([Expr], Expr, [Expr])
deduce = deduceWithAnnotation . annotate

deduceWithAnnotation ∷ Proof → ([Expr], Expr, [Expr])
deduceWithAnnotation (Proof hypos conclusion proofs) = (trHypos, trConclusion, trProofs) where
    alpha = last hypos

    axiom2 a b c = (a :-> b) :-> (a :-> b :-> c) :-> (a :-> c)

    getProofMP ∷ Prop → Map.Map Int Expr → Maybe [Expr]
    getProofMP Prop{getExpr = e, mp = Just (i, j)} m = do
        (dj, di) <- splitImpl $ fromJust $ Map.lookup i m
        return $ reverse [  axiom2 alpha dj di
                 ,  ((alpha :-> (dj :-> di)) :-> (alpha :-> di))
                 ,  alpha :-> di
                 ]

    getProofMP _ _ = Nothing


    trHypos ∷ [Expr]
    trHypos = dropTail 1 hypos

    trConclusion ∷ Expr
    trConclusion = alpha :-> conclusion

    trProofs ∷ [Expr]
    trProofs = step proofs (getMap proofs) [] where
        step ∷ [Prop] → Map.Map Int Expr → [Expr] → [Expr]
        step (p@Prop{getExpr = e}:ps) m stack
            | e == alpha = step ps m (reverse (proofAToA e) ++ stack)
            | isAxiom p || isHypos p = step ps m (reverse [  e :-> alpha :-> e
                                                          ,  e
                                                          ,  alpha :-> e
                                                 ] ++ stack)

            | isMP p = step ps m $ fromJust (getProofMP p m) ++ stack
            | otherwise = error $ "Не доказательство:\n" ++ showCom hypos ++ "|-" ++ show conclusion ++ "\n" ++  showLines proofs
        step [] _ stack = reverse stack

        getMap ∷ [Prop] → Map.Map Int Expr
        getMap props = Map.fromList (map (\prop → (index prop, getExpr prop)) props)

antiDeduce ∷ ([Expr], Expr, [Expr]) → ([Expr], Expr, [Expr])
antiDeduce (hypos, conclusion@(a :-> b), proof) = (
    hypos ++ [a],
    b,
    proof ++ [a, b]
    )
antiDeduce _ = error "antiDeduce: fail argument :("

contraposition ∷ Expr → Expr → [Expr]
contraposition a b =
    let proof'AImplB'NotB'NotA =[ a :-> b                                   -- hypo 1
                                , (a :-> b) :-> (a :-> Not b) :-> Not a     -- ax. 9
                                , (a :-> Not b) :-> Not a                   -- MP
                                , Not b :-> a :-> Not b                     -- ax 1
                                , Not b                                     -- hypo 2
                                , a :-> Not b                               -- MP
                                , Not a                                     -- MP
                                ]
    in third $ (deduce . deduce) ([a :-> b, Not b], Not a, proof'AImplB'NotB'NotA)

proofAOrNotA ∷ Expr → [Expr]
proofAOrNotA a =
    [   a :-> a :| Not a -- Ax.6
    ]
    ++  contraposition a (a :| Not a) ++ -- (a -> a | !a) -> (!(a | !a) -> !a)
    [   Not (a :| Not a) :-> Not a -- MP :1
    ,   Not a :-> a :| Not a -- Ax.7
    ]
    ++  contraposition (Not a) (a :| Not a) ++ -- (!a -> a | !a) -> (!(a | !a) -> !!a)
    [   Not (a :| Not a) :-> Not (Not a) -- MP :2
    ,   (Not (a :| Not a) :-> Not a) :-> (Not (a :| Not a) :-> Not (Not a)) :-> (Not (Not (a :| Not a))) -- Ax.9
    ,   (Not (a :| Not a) :-> Not (Not a)) :-> (Not (Not (a :| Not a))) -- MP (1)
    ,   (Not (Not (a :| Not a))) -- MP (2)
    ,   (Not (Not (a :| Not a))) :-> (a :| Not a) -- Ax.10
    ,   a :| Not a -- MP
    ]
