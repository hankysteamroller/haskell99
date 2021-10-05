module ElevenToTwenty (decodeModified, encodeDirect, encodeModified, EncodeResult(..)) where

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
