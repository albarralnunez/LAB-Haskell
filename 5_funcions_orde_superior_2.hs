flatten :: [[Int]] -> [Int]
--flatten [] = [] 
--flatten (x:xs) = x ++ flatten xs
--flatten l = concat l
flatten (x:xs) = foldl (++) x xs

myLength l = foldr (+) 0 [1 | x <- l]

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

countIn l n = map (\g -> myLength (filter (==n) g)) l 

firstWord l = takeWhile (\x -> x /= ' ') (dropWhile (\x -> x == ' ') l)

