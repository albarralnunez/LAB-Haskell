
absValue x = if x < 0 then -x else x
absValue' x 
    |  x >= 0 = x
    | otherwise = -x
--

power x 0 = 1
power x y = auxpower x x y 

auxpower x y z
    | z == 1 = y
    | otherwise = auxpower x (x*y) (z-1)
--

isPrime 1 = False
isPrime x 
    | divi x (x-1) = False
    | otherwise = True 

divi x y 
    | y == 1 = False
    | x `mod` y == 0 = True
    | otherwise = divi x (y-1) 
--
slowFib 0 = 0
slowFib 1 = 1
slowFib x = sFib 0 1 2 x

--

quickFib 0 = 0
quickFib 1 = 1
quickFib x = fst (epicFib x)
    --sFib 0 1 2 x

---
epicFib 0 = (0,1)
epicFib x = (j, j + i)
    where (i, j) = epicFib (x - 1)

-- Calculate Fibonacci
sFib x y z j
    | z == j = x+y
    | otherwise = sFib y (x+y) (z+1) j 