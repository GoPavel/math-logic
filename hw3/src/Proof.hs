{-# LANGUAGE UnicodeSyntax #-}

module Proof where

import           Data.List
import           Grammar
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
    getProps      ∷ [Prop]
}

instance Show Prop where
    show prop = "(" ++ show (index prop) ++ ") "  ++ (show . getExpr) prop ++ " " ++ getAnnotate prop where
        getAnnotate ∷ Prop → String
        getAnnotate prop = case indexOfAxiom prop of
            (Just i) → "(Сх. акс. " ++ show i ++ ")"
            Nothing → case indexOfHypo prop of
                (Just i) → "(Предп. " ++ show i ++ ")"
                Nothing → case mp prop of
                    (Just (a, b)) → "(M.P. " ++ show a ++ ", " ++ show b ++ ")"
                    Nothing       → "(Не доказано)"



instance Show Proof where
    show (Proof hypos concl props) =
        intercalate "," (map show hypos) ++ "|-" ++
        show concl ++ "\n" ++
        intercalate "\n" (map show props)
