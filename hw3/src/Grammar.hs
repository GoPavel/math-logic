module Grammar where

import           Data.List (intercalate)

infixr 3 :->
infixl 4 :|
infixl 5 :&

data Expr = Expr :& Expr
          | Expr :| Expr
          | Expr :-> Expr
          | Not Expr
          | Var String
          deriving (Ord)

instance Eq Expr where
    (==) (Var a) (Var b)         = a == b
    (==) (Not a) (Not b)         = a == b
    (==) (a1 :-> a2) (b1 :-> b2) = a1 == b1 && a2 == b2
    (==) (a1 :& a2) (b1 :& b2)   = a1 == b1 && a2 == b2
    (==) (a1 :| a2) (b1 :| b2)   = a1 == b1 && a2 == b2
    (==) _ _                     = False

instance Show Expr where
    show (a :& b)   = "(" ++ show a ++ ")&(" ++ show b ++ ")"
    show (a :| b)   = "(" ++ show a ++ ")|(" ++ show b ++ ")"
    show (a :-> b)  = "(" ++ show a ++ ")->(" ++ show b ++ ")"
    show (Not e)    = "!(" ++ show e ++ ")"
    show (Var name) = name

exprFormString ∷ String → Expr
exprFormString s = case parseExpr (alexScanTokens s) of
    Left err   → error "error parse!!!"
    Right expr → expr

splitImpl ∷ Expr → Maybe (Expr, Expr)
splitImpl (a :-> b) = Just (a, b)
splitImpl _         = Nothing
