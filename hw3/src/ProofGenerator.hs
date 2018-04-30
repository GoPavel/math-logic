{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module ProofGenerator where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Deduction       (deduce)
import           ExprUtil        (contraposition, proofAToA, proofAOrNotA)
import           Grammar
import           Utility         (formRight, third)
-- import Data.Sequence as Seq

proofGen ∷ String → Maybe String
proofGen text = do
    text ← return $ removeSpace text
    text ← return $ splitOn2 ('|', '=') text
    leftText ← return $ splitOn1 ',' (head text)
    leftExpr ← return $ map exprFormString leftText
    rightText ← return $ tail text
    rightExpr ← return $ exprFormString rightText
    commonExpr ← return $ moveAllToRight (reverse leftExpr) rightExpr
    -- isProved ← return $
    return undefined --TODO

    where
        moveAllToRight ∷ [Expr] → Expr → Expr
        moveAllToRight (h:hs) acc = h :-> acc
        moveAllToRight [] acc     = acc

        -- check ∷ Expr → Bool
        -- check expr = evaluate

        getVars ∷ [Expr] → Set.Set String
        getVars exprs = Set.fromList $ foldr (++) $ map step exprs where
            step ∷ Expr → [String]
            step expr = case expr of
                a :-> b  → step a ++ step b
                a :| b   → step a ++ step b
                a :& b   → step a ++ step b
                Not a    → step a
                Var name → [name]

recurGen ∷ Expr → [String] → Map.Map String Bool → Either (Map.Map String Bool) [Expr]
recurGen expr (v:vs) values = do
    proofLeft ← recurGen expr vs (insert v False values)
    proofRight ← recurGen expr vs (insert v True values)
    hypos ← return $ f values
    return exclusionOfAdmission (hypos : Var v, expr, proofRight) (hypos: Not (Var v), expr, proofLeft)  where --TODO
        f ∷ Map.Map String Bool → [Expr]
        f m = map (\(str, b) -> if b then Var str else Not (Var str)) (Map.toList m)

recurGen expr [] values =
    case prove values expr of
        Left _      → Left values -- if false then return exception
        Right proof → Right proof -- otherwise return proof

prove ∷ Map.Map String Int → Expr → Either [Expr] [Expr]
prove values (a :| b@(prove values → Right proofB)) = Rigth $
    proofB ++ [
        b :-> (a :| b),
        a :| b
    ]
prove values (a@(prove values -> Right proofA) :| b) = Right $
    proofA ++ [
        a :-> (a :| b),
        a:| b
    ]
prove values (a@(prove values -> Left proofA) :| b@(prove values -> Left proofB)) = Left $
    proofA ++ proofB ++ [
        (a :| b :-> a) :-> (a :| b :-> Not a) :-> Not (a :| b)  -- axiom 9
    ]
    ++ proofAToA a                                          -- a -> a
    ++ formRight (prove values (b :-> a)) ++ [              -- b -> a
        (a :-> a) :-> (b :-> a) :-> (a :| b :-> a),         -- axiom 8
        (b :-> a) :-> (a :| b :-> a),                       -- MP
        a :| b :-> a,                                       -- MP
        (a :| b :-> Not a) :-> Not (a :| b),                -- MP
        Not a :-> a :| b :-> Not a,                         -- axiom 1
        a :| b :-> Not a,                                   -- MP
        Not (a :| b)                                        -- MP
    ]
prove values (a@(prove values -> Right proofA) :& b@(prove values -> Right proofB)) = Right $
    proofA ++ proofB ++ [
        a :-> b :-> a :& b,
        b :-> a :& b,
        a :& b
    ]
prove values (a@(prove values -> Left proofA) :& b) = Left $
    proofA ++ [
        (a :& b :-> a) :-> (a :& b :-> Not a) :-> Not (a :& b),
        a :& b :-> a,
        (a :& b :-> Not a) :-> Not (a :& b),
        Not a :-> a :& b :-> Not a,
        a :& b :-> Not a,
        Not (a :& b)
    ]
prove values (a :& b@(prove values -> Left proofB)) = Left $
    proofB ++ [
        (a :& b :-> b) :-> (a :& b :-> Not b) :-> Not (a :& b),
        a :& b :-> b,
        (a :& b :-> Not b) :-> Not (a :& b),
        Not b :-> a :& b :-> Not b,
        a :& b :-> Not b,
        Not (a :& b)
    ]
prove value (a :-> b@(prove values -> Right proofB)) = Right $
    proofB ++ [
        b :-> a :-> b,
        a :-> b
    ]
prove value (a@(prove values -> Right proofA) :-> b@(prove values -> Left proofB)) = Left $
    proofA ++ proofB ++ [
        ((a :-> b) :-> b) :-> ((a :-> b) :-> Not b) :-> Not (a :-> b) -- axiom 8
    ] ++ third (deduce ([a, Not b, a :-> b], b, [a, a :-> b, b])) ++ [        -- (a -> b) -> b
        ((a :-> b) :-> Not b) :-> Not (a :-> b),                      -- MP
        Not b :-> (a :-> b) :-> Not b,                                -- axiom 1
        (a :-> b) :-> Not b,                                          -- MP
        Not (a :-> b)                                                 -- MP
    ]
prove value (a@(prove value -> Left proofA) :-> b@(prove value -> Left proofB)) = Right $
    contraposition (Not b)  (Not a) ++                  -- (!b -> !a) -> (!!a -> !!b)
    formRight (prove value Not b :-> Not a) ++          -- !b -> !a
    [Not (Not a) :-> Not (Not b)] ++
    third (deduce (
        [Not (Not a) :→ Not (Not b), a], b
        [ Not (Not a) :-> Not (Not b)
        , Not (Not b)
        , Not (Not b) :-> b
        , b
        ])) ++
    [a :-> b]
prove value (Not a@(prove value -> Left proofA)) = Right proofA
prove value (Not a@(prove value -> Right proofA)) = Left $
    proofA ++ proofAToA (Not a) [
        a,                                                          -- hypo
        a :-> Not a :-> a,                                          -- axiom 1
        Not a :-> a                                                 -- MP
        (Not a :-> a) :-> (Not a :-> Not a) :-> Not (Not a),        -- axiom 9
        (Not a :-> Not a) :-> Not (Not a),                          -- MP
         Not (Not a)                                                -- MP
    ]
prove value v@(Var a) | value ! a = Right [v]
                      | not (value ! a) = Left [v]

exclusionOfAdmission ∷ ([Expr], Expr, [Expr]) → ([Expr], Expr, [Expr]) → [Expr]
exclusionOfAdmission trueProof@(hypos : ρ, α, proofΓρtoα) fρlseProof@(hypos : _, _, proofΓnotρtoα) =
    third (deduce trueProof) ++
    third (deduce falseProof) ++
    proofAOrNotA α ++ [
        (ρ :-> α) :-> (Not ρ :-> α) :-> ρ :| Not ρ :-> α,
        (Not ρ :-> α) :-> ρ :| Not ρ :-> α,
        ρ :| Not ρ :-> α,
        α
    ]
