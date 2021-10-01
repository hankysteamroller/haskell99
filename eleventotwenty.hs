module ElevenToTwenty (encodeModified) where

import OneToTen

data EncodeResult a = Single a | Multiple Int a 
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodeResult a]
encodeModified = map getEncodeResult . pack
    where
        getEncodeResult [x] = Single x
        getEncodeResult xs = Multiple (length xs) (head xs)

