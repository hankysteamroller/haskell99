module ElevenToTwenty (decodeModified, dupli, encodeDirect, encodeModified, EncodeResult(..), repli, dropEvery, split, slice) where

import OneToTen

data EncodeResult a = Single a | Multiple Int a 
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodeResult a]
encodeModified = map getEncodeResult . pack
    where
        getEncodeResult [x] = Single x
        getEncodeResult xs = Multiple (length xs) (head xs)

decodeModified :: [EncodeResult a] -> [a]
decodeModified = concatMap decodeEncodeResult
    where
        decodeEncodeResult (Single x) = [x]
        decodeEncodeResult (Multiple len x) = replicate len x

encodeDirect :: (Eq a) => [a] -> [EncodeResult a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect all@(x:xs) = if x == (head xs) then Multiple headDuplicates x:encodeDirect tailAfterDuplicates else Single x:encodeDirect xs
    where
        headDuplicates = length (takeWhile (==x) all)
        tailAfterDuplicates = dropWhile (==x) xs

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

repli :: [a] -> Int -> [a]
repli xs n = xs >>= (replicate n)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [y | (i,y) <- (zip [1..] xs), (mod i n) /= 0]

split :: [a] -> Int -> ([a], [a])
split xs n = (keepSeconds takeWhile splitCond withIndex, keepSeconds dropWhile splitCond withIndex)
    where
        keepSeconds f0 f1 ts = map snd (f0 f1 ts)
        splitCond (i, x) = i <= n
        withIndex = zip [1..] xs

slice :: [a] -> Int -> Int -> [a]
slice xs start end = [x | (i, x) <- (zip [1..] xs), i >= start && i <= end]
