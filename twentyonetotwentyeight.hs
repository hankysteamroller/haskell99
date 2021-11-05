import Data.List (groupBy, sortBy, tails)
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

diff_select :: Int -> Int -> IO [Int]
diff_select n to = rnd_select [1..to] n

rnd_permu :: [a] -> IO [a]
rnd_permu []     = return []
rnd_permu (x:xs) = do
    rand <- randomRIO (0, (length xs))
    rest <- rnd_permu xs
    return $ let (ys,zs) = splitAt rand rest
             in ys++(x:zs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do
    y:xs' <- tails xs
    ys <- combinations (n-1) xs'
    return (y:ys)

lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs xs' -> length xs `compare` length xs')

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy (\xs xs' -> length xs == length xs') . lsort
