import System.Random

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = flip (++) (drop n xs) $ flip (++) [x] $ take (n-1) xs

range :: Int -> Int -> [Int]
range n m
    | n > m = []
    | otherwise = n:range (n+1) m


rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- newStdGen
    return (map ((!!) xs) $ take n $ randomRs (0, (length xs-1)) gen)

rnd_select' :: [a] -> Int -> IO [a]
rnd_select' xs n = newStdGen >>= (\x -> return (rndSelectP x))
    where 
        rndSelectP gen = map ((!!) xs) $ take n $ randomRs (0, (length xs-1)) gen
