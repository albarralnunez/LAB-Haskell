insert l n = [y | y <- l, y <= n] ++ [n] ++ [y | y <- l, y > n]

isort [] = []
isort (x:xs) = insert (isort xs) x
--	where insert l n =  
--		[y | y <- l, y <= n] ++ [n] ++ [y | y <- l, y > n]

remove [n] m
	| n == m = []
	| otherwise = [n]
remove (x:xs) n 
	| x == n = xs
	| otherwise = x:remove xs n   

--minimum l = foldr (>) 0 l

ssort [] = []
ssort l@(x:xs) = 
	let m = minimum l in 
		m : (ssort (remove l m))


--merge :: [a] -> [b] -> [a]
merge l [] = l
merge [] g = g
merge l@(x:xs) g@(y:ys)
	| l <= g = x:merge xs g
	| otherwise = y:merge l ys

--msort :: [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = 
	let m = split l in
	   merge (msort(fst m)) (msort(snd m))

split :: [a] -> ([a],[a])
split []        = ([],[])
split [x]       = ([x],[])
split (x:y:zs)  = (x:xs,y:ys) where (xs,ys) = split zs

