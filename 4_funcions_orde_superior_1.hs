eql l g
	| length l /= length g = False 
	| otherwise = 
		let m = zip l 	g in
		all (\m -> (fst m) == (snd m)) m

prod l = foldr (*) 1 l

prodOfEvens l = foldr (*) 1 (filter (even) l)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
  | factors == []  = [n]
  | otherwise = factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

isPowerOf2 x = all (==2) (primeFactors x)

powersOf2' = 1 : [x | x <- [2..], isPowerOf2 x]

powersOf2 = iterate (*2) 1

scalarProduct l g = 
	let m = zip l g in
	foldr (+) 0 [(fst x) * (snd x) | x <- m]

