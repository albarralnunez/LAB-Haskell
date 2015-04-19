countIf :: (Int -> Bool) -> [Int] -> Int 
countIf f l =  foldr (+) 0 [ 1 | x <- (filter f l)]

pam' :: [Int] -> [Int -> Int] -> [[Int]]
pam' l [] = []
pam' l (x:xs) = map x l : pam l xs

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l g = map (`map` l) g   

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l g = map (\x -> map ($x) g) l 

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl	fx fy n l = foldl fy n (filter fx l)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f l n = takeWhile (`f` n) l ++ [n] ++ dropWhile (`f` n) l

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f [] = []
insertionSort f (x:xs) = insert f (insertionSort f xs) x