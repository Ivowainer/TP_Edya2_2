module ArrSeq where

import qualified Arr as A
--import Arr (!)

instance Seq A where
    emptyS     :: s a
    emptyS = empty 

    singletonS :: a -> s a
    singletonS x = fromList [x] 

    lengthS    :: s a -> Int 
    lengthS = length 

    nthS       :: s a -> Int -> a 
    nthS s n = s A.! n 

    tabulateS  :: (Int -> a) -> Int -> s a
    tabulateS = tabulate

    mapS       :: (a -> b) -> s a -> s b 
    mapS f s
        | lengthS s == 0 = emptyS
        | lengthS s == 1 = Singleton (f (nthS s 1))
        | otherwise = 
            let
                (l, r) = subArray 0 (div (lengthS s) 2) s ||| subArray (div (lengthS s) 2 + 1) (lengthS s) s 
                (ml, mr) = map f l ||| map f r
            in flatten (fromList [ml, mr])

    filterS    :: (a -> Bool) -> s a -> s a 
    filterS f s 
        | lengthS s == 0 = emptyS
        | lengthS s == 1 = if f (nthS s 1) then Singleton (f (nthS s 1)) else emptyS 
        | otherwise = 
            let
                (l, r) = subArray 0 (div (lengthS s) 2) s ||| subArray (div (lengthS s) 2 + 1) (lengthS s) s 
                (ml, mr) = filterS f l ||| filterS f r
            in flatten (fromList [ml, mr])

    appendS    :: s a -> s a -> s a
    appendS s r = flatten fromList [s,r]

    takeS      :: s a -> Int -> s a
    takeS s n = subArray 0 (min n (lengthS s)) s

    dropS      :: s a -> Int -> s a
    dropS s n = subArray (max (length s - n) 0) n s

    -- showtS     :: s a -> TreeView a (s a)
    -- showlS     :: s a -> ListView a (s a)
    -- joinS      :: s (s a) -> s a
    -- reduceS    :: (a -> a -> a) -> a -> s a -> a
    -- scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
    -- fromList   :: [a] -> s a