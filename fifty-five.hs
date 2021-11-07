import Data.List (group, groupBy, sort, sortBy)

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq, Ord)

cbalTree :: Int -> [Tree Char]
cbalTree n = rmdups $ filter (\t -> (nodeWeightDiff t) < 2) $ cbalTree' n [Empty]

cbalTree' :: Int -> [Tree Char] -> [Tree Char]
cbalTree' 0 t = t
cbalTree' n t = cbalTree' (n - 1) (concatMap getFillOptions t)

-- Get all options how to add a leaf to a tree
getFillOptions :: Tree Char -> [Tree Char]
getFillOptions (Empty) = [Branch 'x' Empty Empty]
getFillOptions (Branch x Empty Empty) = [Branch x (Empty) (Branch x Empty Empty), Branch x (Branch x Empty Empty) (Empty)]
getFillOptions (Branch x l r) = map (\xs -> (Branch x xs r)) (getFillOptions l) ++ map (\xs -> (Branch x l xs)) (getFillOptions r)

nodeWeightDiff :: Tree a -> Int
nodeWeightDiff Empty = 0
nodeWeightDiff (Branch x Empty Empty) = 0
nodeWeightDiff (Branch x l r) = abs (nodesCount l - nodesCount r)

nodesCount :: Tree a -> Int
nodesCount (Empty) = 0
nodesCount (Branch x l r) = 1 + (nodesCount l) + (nodesCount r)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Helper to prettyPrint the tree
pp :: Tree Char -> IO ()
pp =  putStrLn . 
    concatMap (++ "\n") . 
    map (\xs -> map (\(_, _, c) -> c) xs) . 
    filter (any (\(_, _, c) -> c /= ' ')) .
    groupBy (\(n0, _, _) (n1, _, _) -> n0 == n1) . 
    sortBy (\(n0, _, _) (n1, _, _) -> n0 `compare` n1) . 
    pp' 0 'S'

-- Add metadata to tree to help pretty-printing
pp' :: Int -> Char -> Tree Char -> [(Int, Char, Char)]
pp' depth side (Empty) = [(depth, side, ' ')]
pp' depth side (Branch x Empty Empty) = [(depth, side, x), (depth + 1, 'L', ' '), (depth + 1, 'R', ' ')]
pp' depth side (Branch x l r) = [(depth, side, x)] ++ pp' (depth + 1) 'L' l ++ pp' (depth + 1) 'R' r

-- Helpers for interactive prompt
leaf = (Branch 'x' Empty Empty)
treeOfThree = (Branch 'x' leaf leaf)
