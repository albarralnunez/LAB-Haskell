--

myLength [] = 0
myLength (x:xs) = (myLength xs) + 1


--

myMaximum [x] = x
myMaximum (x:xs) 
    | myMaximum xs <= x = x
    | otherwise = myMaximum xs

--

average [x] = x
average l@(x:xs) = ((average xs) + x) / myLength l

--

buildPalindrome x = reverse x ++ x

remove' :: [Int] -> [Int] -> [Int]
remove' [] g = []
remove' (x:xs) g
    | (any (==x) g) = remove xs g
    | otherwise = x : remove xs g

remove :: [Int] -> [Int] -> [Int]
remove l g = filter (\x-> all (/=x) g) l 

flatten :: [[Int]] -> [Int]
flatten [] = [] 
flatten (x:xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens l = ((filter (odd) l) , (filter (even) l))

primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors n
  | factors == []  = [n]
  | otherwise = factors ++ primeDivisors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]