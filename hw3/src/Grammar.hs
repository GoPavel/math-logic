module Grammar where

import Data.List (intercalate)

-- data Binop = Impl | Or | And
    -- deriving (Eq, Ord)

-- instance Show Binop where
--   show Impl = "->"
--   show Or   = "|"
--   show And  = "&"

-- data Expr = Binary Binop Expr Expr
--           | Not Expr
--           | Var String
--       deriving (Ord)

data Expr = Expr :& Expr
          | Expr :| Expr
          | Expr :-> Expr
          | Not Expr
          | Var String
          deriving (Ord)

-- instance Eq Expr where
--     (==) (Var a) (Var b) = a == b
--     (==) (Not a) (Not b) = a == b
--     (==) (Binary Impl a1 a2) (Binary Impl b1 b2) = a1 == b1 && a2 == b2
--     (==) (Binary Or a1 a2) (Binary Or b1 b2) = a1 == b1 && a2 == b2
--     (==) (Binary And a1 a2) (Binary And b1 b2) = a1 == b1 && a2 == b2
--     (==) _ _ = False

instance Eq Expr where
    (==) (Var a) (Var b) = a == b
    (==) (Not a) (Not b) = a == b
    (==) (a1 :-> a2) (b1 :-> b2) = a1 == b1 && a2 == b2
    (==) (a1 :& a2) (b1 :& b2) = a1 == b1 && a2 == b2
    (==) (a1 :| a2) (b1 :| b2) = a1 == b1 && a2 == b2
    (==) _ _ = False

-- instance Show Expr where
    -- show (Binary op a b) = "(" ++ show a ++ ")" ++ show op ++ "(" ++ show b ++ ")"
    -- show (Not e) = "!(" ++ show e ++ ")"
    -- show (Var name) = name


instance Show Expr where
    show (a :& b) = "(" ++ show a ++ ")&(" ++ show b ++ ")"
    show (a :| b) = "(" ++ show a ++ ")|(" ++ show b ++ ")"
    show (a :-> b) = "(" ++ show a ++ ")->(" ++ show b ++ ")"
    show (Not e) = "!(" ++ show e ++ ")"
    show (Var name) = name
