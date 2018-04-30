{-# LANGUAGE UnicodeSyntax #-}

module ExprUtil where

import           Grammar
import           Lexer   (alexScanTokens)
import           Parser  (parseExpr)
import           Deduction (deduce)

checkAxiom ∷ Expr → Maybe Int
checkAxiom (a1 :-> b :-> a1)
    | a1 == a2 =                                                Just 1
checkAxiom ((a1 :-> b1) :-> (a2 :-> b2 :-> c1) :-> (a3 :-> c2))
    | a1 == a2 && a2 == a3 && b1 == b2 && c1 == c2 =            Just 2
checkAxiom (a1 :-> b1 :-> a2 :& b2)
    | a1 == a2 && b1 == b2 =                                    Just 3
checkAxiom (a :& b :-> c)
    | a == c =                                                  Just 4
    | b == c =                                                  Just 5
checkAxiom (c :-> a :| b)
    | c == a =                                                  Just 6
    | c == b =                                                  Just 7
checkAxiom ((a1 :-> b1) :-> (c1 :-> b2) :-> (a2 :| c2 :-> b3))
    | a1 == a2 && c1 == c2 && b1 == b2 && b2 == b3 =            Just 8
checkAxiom ((a1 :-> b1) :-> (a2 :-> Not b2) :-> Not a3)
    | a1 == a2 && a2 == a3 && b1 == b2 =                        Just 9
checkAxiom (a1 :-> Not (Not a2))
    | a1 == a2 =                                                Just 10
checkAxiom _  =                                                 Nothing

proofAToA ∷ Expr → [Expr]
proofAToA α = [ α :-> α :-> α ,
                (α :-> (α :-> α)) :-> (α :-> ((α :-> α) :-> α)) :-> (α :-> α),
                (α :-> ((α :-> α) :-> α)) :-> (α :-> α),
                α :-> ((α :-> α) :-> α),
                α :-> α ]

contraposition ∷ Expr → Expr → [Expr]
contraposition a b =
    let proof'AImplB'NotB'NotA =[ a :-> b
                                , (a :-> b) :-> (a :-> Not b) :-> Not a
                                , (a :-> Not b) :-> Not a
                                , Not b :-> a :-> Not b
                                , Not b
                                , a :-> Not b
                                , Not a
                                ]
    in (deduce . deduce) ([a :-> b, Not b], Not a, proof'AImplB'NotB'NotA)

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
