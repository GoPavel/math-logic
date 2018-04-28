{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module ProofGenerator where

import Utility
import Grammar
import qualified Data.Map.Strict as Map
import           Data.List
import           Control.Monad
import           Grammar
import           Lexer           (alexScanTokens)
import           Parser          (parseExpr)
import           Utility
import qualified Data.Set as Set

-- import Data.Sequence as Seq

getExpr ∷ String → Expr
getExpr s = case parseExpr (alexScanTokens s) of
    Left err   → error "error parse!!!"
    Right expr → expr

proofGen ∷ String → Maybe String
proofGen text = do
    text ← return $ removeSpace text
    text ← return $ splitOn2 ('|', '=') text
    leftText ← return $ splitOn1 ',' (head text)
    leftExpr ← return $ map getExpr leftText
    rightText ← return $ tail text
    rightExpr ← return $ getExpr rightText
    commonExpr ← return $ moveAllToRight (reverse leftExpr) rightExpr
    -- isProved ← return $
    return undefined --TODO

    where
        moveAllToRight ∷ [Expr] → Expr → Expr
        moveAllToRight (h:hs) acc = h :-> acc
        moveAllToRight [] acc = acc

        -- check ∷ Expr → Bool
        -- check expr = evaluate

        getVars ∷ [Expr] → Set.Set String
        getVars exprs = Set.fromList $ foldr (++) $ map step exprs where
            step ∷ Expr → [String]
            step expr = case expr of
                a :-> b → step a ++ step b
                a :| b → step a ++ step b
                a :& b → step a ++ step b
                Not a → step a
                Var name → [name]

recurGen ∷ Expr → [String] → Map.Map String Bool → Either (Map.Map String Bool) [Expr]
recurGen expr (v:vs) values = do
    leftSon ← recurGen expr vs (insert v False values)
    rightSon ← recurGen expr vs (insert v True values)
    return undefined -- TODO
recurGen expr [] values =
    case prove values expr of
        Left _ → Left values
        Right proof → Right proof
    -- doIfElse (evaluate expr values)
        -- undefined --TODO
        -- values
    -- where
        -- step (a op b) -- TODO View pattern

-- evaluate ∷ Expr → Map.Map String Bool → Bool
-- evaluate expr values = let
--     eval e = evaluate e values
--     in case expr of
--         a :-> b → Not (eval a) && eval b
--         a :| b → eval a || eval b
--         a :& b → eval a && eval b
--         Not a → not $ eval a
--         Var name → values ! name

-- TODO

aImpla = undefined

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
        (a :| b :→ a) :→ (a :| b :→ Neg a) :→ Neg (a :| b),
        Neg a :-> a :| b :-> Neg a,
        a :| b :-> Neg a
    ]
    ++ aImpla
    ++ formRight (prove values (b :-> a)) ++ [
        (a :→ a) :→ (b :→ a) :→ (a :| b :→ a),
        (b :→ a) :→ (a :| b :→ a),
        a :| b :→ a,
        (a :| b :→ Neg a) :→ Neg (a :| b),
        Neg (a :| b)
    ]
prove values (a@(prove values -> Right proofA) :& b@(prove values -> Right proofB)) = Right $
    proofA ++ proofB ++ [
        a :-> b :-> a :& b,
        b :-> a :& b,
        a :& b
    ]
prove values (a@(prove values -> Left proofA) :& b) = Left $
    proofA ++ [
        (a :& b :→ a) :→ (a :& b :→ Neg a) :→ Neg (a :& b),
        a :& b :→ a,
        (a :& b :→ Neg a) :→ Neg (a :& b),
        Neg a :→ a :& b :→ Neg a,
        a :& b :→ Neg a,
        Neg (a :& b)
    ]
prove values (a :& b@(prove values -> Left proofB)) = Left $
    proofB ++ [
        (a :& b :→ b) :→ (a :& b :→ Neg b) :→ Neg (a :& b),
        a :& b :→ b,
        (a :& b :→ Neg b) :→ Neg (a :& b),
        Neg b :→ a :& b :→ Neg b,
        a :& b :→ Neg b,
        Neg (a :& b)
    ]
prove value (a :-> b@(prove values -> Right proofB)) = Right $
    proofB ++ [
        b :-> a :-> b,
        a :-> b
    ]
prove value (a@(prove values -> Right proofA) :-> b@(prove values -> Left proofB)) = Left $
    proofA ++ proofB ++ [
        undefined --TODO
    ]
prove value (a@(prove value -> Left proofA) :-> b@(prove value -> Left proofB)) = Right $
    undefined -- TODO
prove value (Neg a@(prove value -> Left proofA)) = Right proofA
prove value (Neg a@(prove value -> Right proofA)) = Left $
    proofA ++ [
    undefined
    ]
