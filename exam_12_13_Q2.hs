-- Problema 1: Suma serie
-- Apartat 1.a:
genSum::[Integer]
genSum = scanl (+) 0 (iterate (+1) 1)

--Apartat 1.b
esSum n = n `elem` (takeWhile (<=n) genSum)

--Problema 2: Seleccio de predicats
--selPred n l = (filter (\x -> (x n) == True) l , filter (\x -> (x n) == False) l)
--selPred 8 [even,odd,(>3),(<=9),(==0)]
check n l = map (\x -> x n) l

--Problema 3: Arbres Fibonacci
data Arbre a = Node a (Arbre a) (Arbre a) | Abuit deriving (Show)

arbreFibonacci (Abuit) = 0
arbreFibonacci (Node _ a b) 
	| x +1 == y = 
		if x > y then x else y 
	| otherwise = -1
	where 
		x = 1 + arbreFibonacci a
		y = 1 + arbreFibonacci b
		

{--
NO
let n4 = (Node 4 (Abuit) (Abuit))
let n5 = (Node 5 (Abuit) (Abuit))
let n6 = (Node 6 (Abuit) (Abuit))
let n7 = (Node 7 (Abuit) (Abuit))
let n3 = (Node 3 n6 n7)
let n2 = (Node 2 n4 n5)
let n1 = (Node 1 n2 n3)

SI
let a2 = (Node 2 (Abuit) (Abuit))
let a1 = (Node 1 a2 (Abuit))
--}