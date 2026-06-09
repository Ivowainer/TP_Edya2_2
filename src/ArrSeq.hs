module ArrSeq where

import qualified Arr as A
import Seq
import Par
import Arr ((!))
import Seq (TreeView(EMPTY))

instance Seq A.Arr where
    emptyS :: A.Arr a
    emptyS = A.empty 

    singletonS :: a -> A.Arr a
    singletonS x = A.fromList [x] 

    lengthS :: A.Arr a -> Int 
    lengthS = A.length 

    nthS :: A.Arr a -> Int -> a 
    nthS s n = s ! n

    tabulateS :: (Int -> a) -> Int -> A.Arr a
    tabulateS = A.tabulate

    mapS :: (a -> b) -> A.Arr a -> A.Arr b 
    mapS f s
        | lengthS s == 0 = emptyS
        | lengthS s == 1 = singletonS (f (nthS s 0))
        | otherwise = 
            let
                (l, r) = A.subArray 0 (div (lengthS s) 2) s ||| A.subArray (div (lengthS s) 2 + 1) (lengthS s) s 
                (ml, mr) = mapS f l ||| mapS f r
            in A.flatten (A.fromList [ml, mr])

    filterS :: (a -> Bool) -> A.Arr a -> A.Arr a 
    filterS f s 
        | lengthS s == 0 = emptyS
        | lengthS s == 1 = if f (nthS s 0) then singletonS (nthS s 0) else emptyS 
        | otherwise = 
            let
                (l, r) = A.subArray 0 (div (lengthS s) 2) s ||| A.subArray (div (lengthS s) 2 + 1) (lengthS s) s 
                (ml, mr) = filterS f l ||| filterS f r
            in A.flatten (A.fromList [ml, mr])

    appendS :: A.Arr a -> A.Arr a -> A.Arr a
    appendS s r = A.flatten (A.fromList [s,r])

    takeS :: A.Arr a -> Int -> A.Arr a
    takeS s n = A.subArray 0 (min n (lengthS s)) s

    dropS :: A.Arr a -> Int -> A.Arr a
    dropS s n = A.subArray n (lengthS s - n)  s

    showtS :: A.Arr a -> TreeView a (A.Arr a)
    showtS arr
        | lengthS arr == 0 = EMPTY
        | lengthS arr == 1 = ELT (nthS arr 0)
        | otherwise       = NODE (takeS arr m) (dropS arr m)
            where m = div (lengthS arr) 2
    
    showlS :: A.Arr a -> ListView a (A.Arr a) 
    showlS arr
        | lengthS arr == 0 = NIL
        | otherwise        = CONS (nthS arr 0) (dropS arr 1)

    joinS :: A.Arr (A.Arr a) -> A.Arr a
    joinS = A.flatten

    -- reduceS :: (a -> a -> a) -> a -> A.Arr a -> a
    -- scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
    
    fromList :: [a] -> A.Arr a
    fromList = A.fromList

{- ===== AUX ===== -}
contr :: (a -> a -> a) -> A.Arr a -> A.Arr a
contr f arr 
    | A.length arr == 0 = A.empty
    | A.length arr == 1 = singletonS (nthS arr 0)
    | A.length arr == 2 = singletonS (f (nthS arr 0) (nthS arr 1))
    | otherwise        = 
        let
            idx = 2 ^ ilg (A.length arr - 1)
            (contraction, list) = contr f (takeS arr 2) ||| contr f (dropS arr 2)
        in  appendS contraction list

-- contr :: (a -> a -> a) -> A.Arr a -> A.Arr a
-- contr f arr 
--     | A.length arr == 0 = A.empty
--     | A.length arr == 1 = singletonS (nthS arr 0)
--     | A.length arr == 2 = singletonS (f (nthS arr 0) (nthS arr 1))
--     | otherwise        = 
--         let
--             idx = 2 ^ ilg (A.length arr - 1)
--             (l, r) = contr f (takeS arr idx) ||| contr f (dropS arr idx)
--         in  appendS l r

-- Costo: O(1)
ilg :: Int -> Int
ilg x = floor (logBase 2 (fromIntegral x))