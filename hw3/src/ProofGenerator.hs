{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module ProofGenerator(proofGen, {-- debug, --} proofAtoNotNotA) where

import           Annotation      (annotate, proofAToA)
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Deduction       (antiDeduce, contraposition, deduce,
                                  proofAOrNotA)
import           Grammar
import           Utility
-- import           Proof
-- import Data.Sequence as Seq

-- debug ∷ String
-- debug = show $ annotate $ deduce ([a :-> b, Not b], Not a, proof'AImplB'NotB'NotA)
-- debug = showLines $ contraposition (Var "A") (Var "B")
-- debug =  show $ annotate ([Not a], expr, (fromRight $ prove values expr))
-- debug = showLines $ exclusionOfAdmission
-- debug = showLines $ fromRight $ prove values expr
-- debug = fromLeft $ proofGen "B,W|=A->B"
-- debug = show $ moveAllToRight (reverse [b, w]) (a:->b)
    -- ([a], expr, (fromRight $ prove Map.fromList [("A", True)] expr))
    -- ([Not a], expr, (fromRight $ prove Map.fromList [("A", False)] expr))
    -- where
        -- expr = b :-> w :-> a :-> b
        --
        -- w = Var "W"
        -- a = Var "A"
        -- b = Var "B"
        --
        -- -- a -> b, !b |- !a
        -- proof'AImplB'NotB'NotA =    [ a :-> b                                   -- hypo 1
        --                             , (a :-> b) :-> (a :-> Not b) :-> Not a     -- ax. 9
        --                             , (a :-> Not b) :-> Not a                   -- MP
        --                             , Not b :-> a :-> Not b                     -- ax 1
        --                             , Not b                                     -- hypo 2
        --                             , a :-> Not b                               -- MP
        --                             , Not a                                     -- MP
        --                             ]
        --
        -- values = Map.fromList [("A", True), ("B", False), ("W", True)]

proofGen ∷ String → Either String String
proofGen text = do
    text ← return $ removeSpace text
    text ← return $ splitOn2 ('|', '=') text
    leftText ← return $ splitOn1 ',' (head text)
    leftExpr ← return $ map exprFormString leftText
    rightText ← return $ last text
    rightExpr ← return $ exprFormString rightText
    commonExpr ← return $ moveAllToRight (reverse leftExpr) rightExpr
    proofCommonExpr ← either (Left . getMsg) Right (recurGen commonExpr (Set.toList $ getVars $ leftExpr ++ [rightExpr]) Map.empty)
    (hypos, conclusion, proof) ← return $ iterate antiDeduce ([], commonExpr, proofCommonExpr) !! length leftExpr
    return $ (intercalate "," (map show hypos)) ++ "|-" ++ show conclusion ++ "\n" ++ (intercalate "\n" $ map show proof)

    where
        moveAllToRight ∷ [Expr] → Expr → Expr
        moveAllToRight (h:hs) acc = moveAllToRight hs (h :-> acc)
        moveAllToRight [] acc     = acc

        getVars ∷ [Expr] → Set.Set String
        getVars exprs = Set.fromList $ (foldr (++) []) $ (map step exprs) where
            step ∷ Expr → [String]
            step expr = case expr of
                a :-> b  → step a ++ step b
                a :| b   → step a ++ step b
                a :& b   → step a ++ step b
                Not a    → step a
                Var name → [name]

        getMsg ∷ Map.Map String Bool → String
        getMsg m = "Высказывание ложно при " ++
            (intercalate ", " $ map (\(str, b) -> str ++ if b then "=И" else "=Л" ) $ Map.toList m)

recurGen ∷ Expr → [String] → Map.Map String Bool → Either (Map.Map String Bool) [Expr]
recurGen expr (v:vs) values = do
    proofLeft ← recurGen expr vs (Map.insert v False values)
    proofRight ← recurGen expr vs (Map.insert v True values)
    hypos ← return $ map (\(str, b) ->
                if b then Var str else Not (Var str)) (Map.toList values)
    return $ exclusionOfAdmission (hypos ++ [Var v], expr, proofRight) (hypos ++ [Not (Var v)], expr, proofLeft)

recurGen expr [] values =
    case prove values expr of
        Left _      → Left values -- if false then return exception
        Right proof → Right proof -- otherwise return proof

prove ∷ Map.Map String Bool → Expr → Either [Expr] [Expr]
prove values (a :| b@(prove values → Right proofB)) = Right $
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
    ++ fromRight (prove values (b :-> a)) ++ [              -- b -> a
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
prove values (a :-> b@(prove values -> Right proofB)) = Right $
    proofB ++ [
        b :-> a :-> b,
        a :-> b
    ]
prove values (a@(prove values -> Right proofA) :-> b@(prove values -> Left proofB)) = Left $
    proofA ++ proofB ++ [
        ((a :-> b) :-> b) :-> ((a :-> b) :-> Not b) :-> Not (a :-> b) -- axiom 8
    ] ++ third (deduce ([a, Not b, a :-> b], b, [a, a :-> b, b])) ++ [        -- (a -> b) -> b
        ((a :-> b) :-> Not b) :-> Not (a :-> b),                      -- MP
        Not b :-> (a :-> b) :-> Not b,                                -- axiom 1
        (a :-> b) :-> Not b,                                          -- MP
        Not (a :-> b)                                                 -- MP
    ]
-- !a, !b |- a -> b
prove values (a@(prove values -> Left proofA) :-> b@(prove values -> Left proofB)) = Right $
    contraposition (Not b)  (Not a) ++                     -- (!b -> !a) -> (!!a -> !!b)
    fromRight (prove values (Not b :-> Not a)) ++          --  !a, !b |- !b -> !a
    [Not (Not a) :-> Not (Not b)] ++                       -- !!a -> !!b
    third (deduce (                                        -- !!a -> !!b, a |- b
        [Not (Not a) :-> Not (Not b), a],
        b,
        proofAtoNotNotA a ++ [ Not (Not a) :-> Not (Not b)
        , Not (Not b)
        , Not (Not b) :-> b
        , b
        ])) ++
    [a :-> b]
prove values (Not a@(prove values -> Left proofA)) = Right proofA
prove values (Not a@(prove values -> Right proofA)) = Left $ proofA ++ (proofAtoNotNotA a)
prove values v@(Var a) | values Map.! a = Right [v]
                       | not (values Map.! a) = Left [Not v]

proofAtoNotNotA ∷ Expr → [Expr]
proofAtoNotNotA α = proofAToA (Not α) ++ [
        α,                                                          -- hypo
        α :-> Not α :-> α,                                          -- axiom 1
        Not α :-> α,                                                -- MP
        (Not α :-> α) :-> (Not α :-> Not α) :-> Not (Not α),        -- axiom 9
        (Not α :-> Not α) :-> Not (Not α),                          -- MP
        Not (Not α)                                                -- MP
    ]

exclusionOfAdmission ∷ ([Expr], Expr, [Expr]) → ([Expr], Expr, [Expr]) → [Expr]
exclusionOfAdmission trueProof@(hypos, α, _) falseProof@(_, _, _) = let ρ = last hypos in
    third (deduce trueProof) ++     -- ρ  -> α
    third (deduce falseProof) ++    -- !ρ -> α
    proofAOrNotA ρ ++ [
        (ρ :-> α) :-> (Not ρ :-> α) :-> ρ :| Not ρ :-> α,
        (Not ρ :-> α) :-> ρ :| Not ρ :-> α,
        ρ :| Not ρ :-> α,
        α
    ]
exclusionOfAdmission (a1, a2, a3) (b1, b2, b3) = error ("exclusionOfAdmission fail :(\n actual arguments: "
    ++ show a1 ++ "|-" ++ show a2 ++ "\n" ++ show b1 ++ "|-" ++ show b2)
