module ListSeq where

import Seq
import Par

instance Seq [] where 
    emptyS :: [a]
    emptyS = []

    singletonS :: a -> [a]
    singletonS a = [a]

    lengthS :: [a] -> Int
    lengthS [] = 0
    lengthS (x : xs) = 1 + lengthS xs

    nthS :: [a] -> Int -> a
    nthS xs i = xs !! i

    tabulateS :: (Int -> a) -> Int -> [a]
    tabulateS _ 0 = []
    tabulateS f n = tabulateSAux 0
        where
            tabulateSAux i = if i == n 
                                then [] 
                                else    let (x, xs) = f i ||| tabulateSAux (i+1)
                                        in x : xs
    
    mapS :: (a -> b) -> [a] -> [b]
    mapS _ [] = []
    mapS f (x : xs) = let (y, ys) = f x ||| mapS f xs 
                      in y : ys

    filterS :: (a -> Bool) -> [a] -> [a]
    filterS f [] = []
    filterS f (x : xs) = let (b, ys) = f x ||| filterS f xs
                         in if b then x : ys else ys

    appendS :: [a] -> [a] -> [a]
    appendS [] ys = ys
    appendS (x : xs) ys = x : (appendS xs ys)

    {- TODO: CONSULTA: PREGUNTAR A LA CATEDRA SI TAKES = TAKE ES VALIDO, DROPS = DROP ES VALIDO, ETC-}
    takeS :: [a] -> Int -> [a]
    takeS [] _ = []
    takeS _ 0 = []
    takeS xs n = takeSAux xs 0
        where
            takeSAux [] _ = []
            takeSAux (x : xs) i = if i == n
                                    then []
                                    else x : takeSAux xs (i+1)

    dropS :: [a] -> Int -> [a]
    dropS [] _ = []
    dropS _ 0 = []
    dropS xs n = dropSAux xs 0
        where
            dropSAux [] _ = []
            dropSAux (x : xs) i = if i >= n
                                    then x : dropSAux xs (i+1)
                                    else dropSAux xs (i+1)

    showtS :: [a] -> TreeView a [a]
    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS xs = NODE (takeS xs m) (dropS xs m)
        where 
            m = div (length xs) 2
    
    showlS :: [a] -> ListView a [a]
    showlS [] = NIL
    showlS (x : xs) = CONS x xs

    joinS :: [[a]] -> [a]
    joinS [] = []
    joinS (xs : xss) = appendS xs (joinS xss)

    reduceS :: (a -> a -> a) -> a -> [a] -> a
    reduceS _ b [] = b
    reduceS f b [x] = f b x
    reduceS f b xs = 
        let 
            contraccion = contr f xs
        in reduceS f b contraccion

    scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
    scanS _ b [] = ([b], b)
    scanS f b [x] = ([b], f b x)
    scanS f b s =
        let
            contraccion = contr f s
            (redu, t) = scanS f b contraccion
        in (algoFunc redu (length s), t)
        where
            algoFunc redu 0 = []
            algoFunc redu i | even i        = nthS redu ((div i 2) - 1) : (algoFunc redu (i-1)) 
                            | otherwise     = f (nthS redu (div i 2)) (nthS s (i-1)) : (algoFunc redu (i-1))


{- ===== AUX ===== -}
contr :: (a -> a -> a) -> [a] -> [a]
contr f [] = []
contr f [x] = [x]
contr f (x : y : xs) = 
    let
        (r, rs) = f x y ||| contr f xs
    in r : rs
