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
recurGen expr (v:vs) mapOfValue = do
    leftSon ← recurGen expr vs (insert v False mapOfValue)
    rightSon ← recurGen expr vs (insert v True mapOfValue)
    return undefined -- TODO
recurGen expr [] mapOfValue = doIfElse (evaluate expr mapOfValue)
        undefined --TODO
        mapOfValue
    where
        step (a op b) -- TODO View pattern

evaluate ∷ Expr → Map.Map String Bool → Bool
evaluate expr mapOfValue = let
    eval e = evaluate e mapOfValue
    in case expr of
        a :-> b → Not (eval a) && eval b
        a :| b → eval a || eval b
        a :& b → eval a && eval b
        Not a → not $ eval a
        Var name → mapOfValue ! name
