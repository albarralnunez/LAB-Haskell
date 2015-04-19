-- Problema 1: Lambdes i fold
-- sum2 [[3,-2,6],[8],[2,5,6],[]] retorna 28
sum2 l = foldl (+) 0 (concat l)

-- Problema 2: Map extra
fsmap n l = foldl (\z y -> y z) n l

-- Problema 3: No divisible
-- knoDiv 4 [2,3,5] retorna 17
knoDiv n l = [x | x <- [1..], (divi x l)] !! n
divi x l = not (any (==0) (map (mod x) l))