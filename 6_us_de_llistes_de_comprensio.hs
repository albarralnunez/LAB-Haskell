myMap' :: (a->b) -> [a] -> [b]
myMap' f l = [f x | x <- l]

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f l = [x | x <- l, f x]

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' f l g = [f (fst x) (snd x) | x <- zip l g]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l g = [ (x,y) | x <- l, y <- g, x `mod` y == 0]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

--myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f n [] = n
myFoldl f n [] = n
myFoldl f n (x:xs) = f n (myFoldl f x xs) 

--myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f n [] = n
myFoldr f n [] = n
myFoldr f n [x] = f x n
myFoldr f n (x:xs) = f (myFoldl f x xs) n  

myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : [f x | x <- myIterate f n ]  

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil fx fy n
	| (fx n) == True = n
	| otherwise = myUntil fx fy (fy n)

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) 
	| f x == True = x : (myFilter f xs)
	| otherwise = (myFilter f xs)

myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = True
myAll f l = myFoldl (&&) True (myMap f l)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f l = myFoldl (||) False (myMap f l)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip [] l = []
myZip l [] = []
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] [] = []
myZipWith f [] l = []
myZipWith f l [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)