module OneToTen (pack) where

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast [] = error "No but last for empty list!"
myButLast [x] = error "No but last for list with single item!"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Invalid empty list"
elementAt xs i
  | i < 1 = error "Index out of bounds"
  | i == 1 = head xs
  | otherwise = elementAt (tail xs) (i - 1)

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1) 0

myLength' :: [a] -> Int
myLength' xs = sum [1 | _ <- xs]

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' = reverse >>= (==)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress [x,y] = if x == y then [x] else [x,y]
compress (x:y:xs) = if x == y then compress (y:xs) else x:(compress (y:xs))

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack
