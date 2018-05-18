{-# LANGUAGE UnicodeSyntax #-}

module Lattice(check, debug) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Utility

data Relation = L | G | N | E
    deriving (Show, Eq)

sumFail (a, b) = "Операция \'+\' не определена: " ++ show a ++ "+" ++ show b
timesFail (a, b) = "Операция \'*\' не определена: " ++ show a ++ "*" ++ show b
distrFail (a, b, c) = "Нарушается дистрибутивность: "
    ++ show a ++ "*(" ++ show b ++ "+" ++ show c ++ ")"
implFail (a, b) = "Операция \'->\' не определена: " ++ show a ++ "->" ++ show b
boolableFail a = "Не булева алгебра: " ++ show a ++ "+~" ++ show a

-- String{graph} → String{answer}
check ∷ Map.Map Int [Int] → String
check graph = case check' of
    (Left msg) -> msg
    (Right msg) -> msg
    where
        check' ∷ Either String String
        check' = do
            let relTable = getRelationTable graph
            let vs = Map.keys graph
            sumTable ← checkSum vs relTable
            timesTable ← checkTimes vs relTable
            let debugMessages = "\n\n" ++ showLines (Map.toList sumTable) ++
                                "\n\n" ++ showLines (Map.toList timesTable)
            isDistr ← checkDistr vs sumTable timesTable
            return $ "Булева алгебра" ++ debugMessages


debug ∷ Map.Map Int [Int] → String
-- debug g = showLines $ Map.toList $ getRelationTable g
debug g = undefined
--     let rel = getRelationTable g
--     in case checkTimes g rel of
--         (Left str) → str
--         (Right timesTable) → showLines $ Map.toList timesTable
-- debug g =
    -- let rel = getRelationTable g
    -- in case checkSum g rel of
    --     (Left str) → str
    --     (Right sumTable) → showLines $ Map.toList sumTable
-- debug g =
--     let rel = getRelationTable g
--         sumTable = checkSum g rel
--         timesTable = checkTimes g rel
--     in case checkDistr g (fromRight sumTable) (fromRight timesTable) of
--         (Left msg) ->  msg
--         (Right msg) -> msg


checkImpl ∷ [Int] → Map.Map (Int, Int) Relation → Map.Map (Int, Int) Int → (Int, Int) →
    Either String (Map.Map (Int, Int) Int)
checkImpl vs rel timesT (a, b) = foldl impl' (Right Map.empty) (listSqr vs)
    where
        impl' ∷ Either String (Map.Map (Int, Int) Int) → (Int, Int) → Either String (Map.Map (Int, Int) Int)
        impl' acc@(Left _) _ = acc
        impl' acc@(Right t) pair@(a, b) =
            let candidates = filter (\x -> isGreaterEq rel (timesT Map.! (x, a), b)) vs
            in case maxCandidate rel candidates of
                Nothing → Left $ implFail pair
                (Just m) → Right $ Map.insert pair m t

checkBoolable ∷ [Int] → Map.Map (Int, Int) Relation
                      → Map.Map (Int, Int) Int
                      → Map.Map (Int, Int) Int → Either String String
checkBoolable vs rel sumT impl = foldl check (Right "Ok") vs
    where
        check ∷ Either String String → Int → Either String String
        check acc@(Left _) _ =  acc
        check acc@(Right _) a =
            let left =  impl Map.! (a, zero)
            in if left == one
                then Right "Ok"
                else Left $ boolableFail a

        zero ∷ Int
        zero = fromJust $ minCandidate rel vs

        one ∷ Int
        one = fromJust $ maxCandidate rel vs

getRelationTable ∷ Map.Map Int [Int] → Map.Map (Int, Int) Relation
getRelationTable graph =
    let vertices = Map.keys graph
        initTable = Map.fromList $ zip (listSqr vertices) (repeat N)
    in foldl accumulation initTable vertices
    where
        accumulation ∷ Map.Map (Int, Int) Relation → Int → Map.Map (Int, Int) Relation
        accumulation rel v =
            let pairsLessV = zip (repeat v) (Set.toList $ dfs graph v Set.empty)
                pairsGreaterV = zip (Set.toList $ dfs graph v Set.empty) (repeat v)
                rel' = foldl updateLess rel pairsLessV
                rel'' = foldl updateGreater rel' pairsGreaterV
            in rel''

        dfs ∷ Map.Map Int [Int] → Int → Set.Set Int → Set.Set Int
        dfs graph v used =
            if v `Set.member` used
                then used
                else foldl (\used v → dfs graph v used) (v `Set.insert` used) (graph Map.! v)

        updateGreater ∷ Map.Map (Int, Int) Relation → (Int, Int) → Map.Map (Int, Int) Relation
        updateGreater rel pair = case rel Map.! pair of
           L → Map.insert pair E rel
           G → rel
           N → Map.insert pair G rel
           E → rel

        updateLess ∷ Map.Map (Int, Int) Relation → (Int, Int) → Map.Map (Int, Int) Relation
        updateLess rel pair = case rel Map.! pair of
          L → rel
          G → Map.insert pair E rel
          N → Map.insert pair L rel
          E → rel

isLess rel pair = rel Map.! pair == L

isGreater rel pair = rel Map.! pair == G

isEq rel pair = rel Map.! pair == E

isNon rel pair = rel Map.! pair == N

isGreaterEq rel pair = isGreater rel pair || isEq rel pair

isLessEq rel pair = isLess rel pair || isEq rel pair

minCandidate ∷ Map.Map (Int, Int) Relation → [Int] → Maybe Int
minCandidate rel cs = foldl min' Nothing cs
    where
        min' ∷ Maybe Int → Int → Maybe Int
        min' curMin@(Just m) x = case rel Map.! (m, x) of
            L → curMin
            G → Just x
            E → Nothing
            N → Nothing
        min' Nothing x = Just x

-- checkSum (listSqr vs) ab rel
checkSum ∷  [Int] → Map.Map (Int, Int) Relation → Either String (Map.Map (Int, Int) Int)
checkSum vs rel = foldl evalSumPair (Right Map.empty) pairs
    where
        pairs ∷ [(Int, Int)]
        pairs = listSqr vs

        candidates ∷ (Int, Int) → [Int]
        candidates (a, b) =
            let predicate c = isGreaterEq rel (c, a) && isGreaterEq rel (c, b)
            in filter predicate vs

        checkUniqueMin ∷ Int → [Int] → Bool
        checkUniqueMin m xs = all (\x → isGreater rel (x, m) || x == m) xs

        evalSumPair ∷ Either String (Map.Map (Int, Int) Int) → (Int, Int) → Either String (Map.Map (Int, Int) Int)
        evalSumPair sums@(Left msg) _ = sums
        evalSumPair sums@(Right sumTable) pair =
            let mins = candidates pair
            in case minCandidate rel mins of
                Nothing  → Left $ sumFail pair
                -- (Just m) → if True
                (Just m) → if checkUniqueMin m mins
                    then Right $ Map.insert pair m sumTable
                    else Left $ sumFail pair

maxCandidate ∷ Map.Map (Int, Int) Relation → [Int] → Maybe Int
maxCandidate rel cs = foldl max' Nothing cs
    where
        max' ∷ Maybe Int → Int → Maybe Int
        max' curMax@(Just m) x = case rel Map.! (m, x) of
            L → Just x
            G → curMax
            E → Nothing
            N → Nothing
        max' Nothing x = Just x

checkTimes ∷ [Int] → Map.Map (Int, Int) Relation → Either String (Map.Map (Int, Int) Int)
checkTimes vs rel = foldl evalTimesPair (Right Map.empty) pairs
    where
        pairs ∷ [(Int, Int)]
        pairs = listSqr vs

        candidates ∷ (Int, Int) → [Int]
        candidates (a, b) =
            let predicate x = isLessEq rel (x, a) && isLessEq rel (x, b)
            in filter predicate vs

        checkUniqueMax ∷ Int → [Int] → Bool
        checkUniqueMax m xs = all (\x -> isLess rel (x, m) || x == m) xs

        evalTimesPair ∷ Either String (Map.Map (Int, Int) Int) → (Int, Int) → Either String (Map.Map (Int, Int) Int)
        evalTimesPair times@(Left msg) _ = times
        evalTimesPair times@(Right timesTable) pair =
            let maxs = candidates pair
            in case maxCandidate rel maxs of
                Nothing → Left $ timesFail pair
                (Just m) → if checkUniqueMax m maxs
                    then Right $ Map.insert pair m timesTable
                    else Left $ timesFail pair

checkDistr ∷ [Int] → Map.Map (Int, Int) Int
                   → Map.Map (Int, Int) Int → Either String String
checkDistr vs sumTable timesTable = case foldl checkTriple Nothing triples of
    (Just str) → Left str
    Nothing → Right "Ok"
    where
        triples ∷ [(Int, Int, Int)]
        triples = listCube vs

        -- a*(b + c) ?= a*b + a*c
        checkTriple ∷ Maybe String → (Int, Int, Int) → Maybe String
        checkTriple Nothing triple@(a, b, c) =
            let left = timesTable Map.! (a, sumTable Map.! (b, c))
                right = sumTable Map.! (timesTable Map.! (a, b), timesTable Map.! (a, c))
            in if left == right
                then Nothing
                else Just $ distrFail triple
        checkTriple msg@(Just _) _ = msg
