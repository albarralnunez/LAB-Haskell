{--
Un arbre amb elements de tipus a és, o bé un arbre buit, 
o bé un node que arrela un element (de tipus a) amb dos altres arbres.
La declaració deriving (Show) permet mostrar els arbres senzillament.
--}
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

{--
Feu una funció size :: Tree a -> Int que, 
donat un arbre, retorni la seva talla, és a dir,
el nombre de nodes que conté.
--}
size :: Tree a -> Int
size (Empty) = 0
size (Node _ b c) = 1 + size b + size c

{--
Feu una funció height :: Tree a -> Int 
que, donat un arbre, retorni la seva alçada, 
assumint que els arbres buits tenen alçada zero.
--}
height :: Tree a -> Int
height (Empty) = 0
height (Node _ b c) 
	| x > y = x
	| otherwise = y 
	where
		x = height b + 1
		y = height c + 1

{--
Feu una funció equal :: Eq a => Tree a -> Tree a -> Bool
que, donat dos arbres, indiqui si són el mateix.
--}
equal :: Eq a => Tree a -> Tree a -> Bool
equal (Empty) (Empty) = True
equal (Empty) a = False
equal a (Empty) = False
equal t1@(Node a b c) t2@(Node d e f) 
	| (size t1) /= (size t2) = False
	| otherwise = 
		if a == d then equal b e && equal c f
		else False 

{--
Feu una funció isomorphic :: Eq a => Tree a -> Tree a -> Bool que, 
donat un arbres, indiqui si són el isomorfs, és a dir, 
si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
--}
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic (Empty) (Empty) = True
isomorphic (Empty) a = False
isomorphic a (Empty) = False
isomorphic t1@(Node a b c) t2@(Node d e f)
	| (size t1) /= (size t2) = False
	| otherwise = 
		if a == d then 
			equal b e && equal c f ||
			equal b f && equal c e
		else False 
{--
Feu una funció preOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en pre-ordre.
--}
preOrder :: Tree a -> [a]
preOrder (Empty) = []
preOrder (Node a b c) = a : (preOrder b) ++ (preOrder c)

{--
Feu una funció postOrder :: Tree a -> [a] que, 
donat un arbre, retorni el seu recorregut en post-ordre.
--}
postOrder :: Tree a -> [a]
postOrder a = reverse (postOrderAux a)
postOrderAux :: Tree a -> [a]
postOrderAux (Empty) = []
postOrderAux (Node a b c) = a : (postOrderAux c) ++ (postOrderAux b) 

{--
Feu una funció inOrder :: Tree a -> [a] que, 
donat un arbre, retorni el seu recorregut en in-ordre.
--}
inOrder :: Tree a -> [a]
inOrder (Empty) = []
inOrder (Node a b c) = (inOrder b) ++ a : (inOrder c)

{--
Feu una funció breadthFirst :: Tree a -> [a] que,
donat un arbre, retorni el seu recorregut per nivells.
--}

breadthFirst :: Tree a -> [a]
breadthFirst a = breadthFirstAux [a]

breadthFirstAux :: [Tree a] -> [a]
breadthFirstAux [] = []
breadthFirstAux (Empty:xs) = breadthFirstAux xs
breadthFirstAux ((Node a b c):xs) = a : breadthFirstAux (xs ++ [b,c])

--	breadthFirstAux b a:l
--	| otherwise = breadthFirstAux c True

{--
Feu una funció build :: Eq a => [a] -> [a] -> Tree a que,
donat el recorregut en pre-ordre d’un arbre i el recorregut 
en in-ordre del mateix arbre, retorni l’arbre original.
 Assumiu que l’arbre no té elements repetits.
--}
build :: Eq a => [a] -> [a] -> Tree a
build [] [] = (Empty)
build p@(x:xs) i@(y:ys) = Node x (build lx ly) (build rx ry) 
	where 
		(ly, (_:ry)) = span (/=x) i
		(lx, rx) = splitAt (length ly) xs  

{--
Feu una funció overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a que,
 donats dos arbres, retorni la seva superposició utilitzant una funció. 
 Superposar dos arbres amb una funció consisteix en posar els dos arbres 
 l’un damunt de l’altre i combinar els nodes doble resultants amb la funció 
 donada o deixant els nodes simples tal qual.
--}
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap g (Empty) (Empty) = (Empty)
overlap g a (Empty) = a
overlap g (Empty) a = a 
overlap g (Node a b c) (Node d e f) = 
	Node (g a d) (overlap g b e) (overlap g c f)
