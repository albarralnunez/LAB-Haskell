ones :: [Integer]
ones = [1 | x <- [1..]]

nats :: [Integer]
nats = [x | x <- [0..]]

merge l [] = l
merge [] g = g
merge (x:xs) (y:ys) = x : y : merge xs ys

ints :: [Integer]
ints = 0: [x |  x <- merge [1..] [-1,-2..]]

-- 0,1,3,6,10,15,21,28,…
triangulars :: [Integer]
triangulars = auxtri 0
auxtri n = ( (n * (n + 1)) `div` 2) : auxtri (n+1) 

--factorials :: [Integer]
--factorials = filter (\x -> (n `mod` x) == 0) [2 .. n-1]

fibs :: [Integer]
fibs = 0 : 1 : makefib 0 1 
makefib n m = (n + m) : makefib m (n+m)

isPrime 1 = False
isPrime x 
    | divi x (x-1) = False
    | otherwise = True 

divi x y 
    | y == 1 = False
    | x `mod` y == 0 = True
    | otherwise = divi x (y-1) 

primes :: [Integer]
primes = makeprime 0
makeprime n 
	| isPrime n == True = n : makeprime (n + 1)
	| otherwise = makeprime (n + 1)

prime_factors 1 = []
prime_factors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
hammings = [ x | x <- [1..], all (\x -> (x==5 || x==2 || x==3)) (prime_factors x)]

{--
lookNsay :: [Integer]
tar0aglia :: [[Integer]]
--}