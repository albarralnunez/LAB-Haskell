genPairs l g t = 
	[(x,y) | x <- l, y <- g, 
	any (==x) t && not(any (==y) t) ||
	not(any (==x) t) && any (==y) t]

nodup l = foldl (\x y -> if (elem y x) then x else x ++ [y]) [] l

data Arbre a = Node a (Arbre a) (Arbre a) | Abuit
	deriving (Show)

ttake (Abuit) _ = (Abuit)
ttake _ 0 = (Abuit)
ttake (Node a b c) n = Node a (ttake b (n-1)) (ttake c (n-1))

inftree = inftreeAux 1
inftreeAux n = Node n (inftreeAux (n + 1)) (inftreeAux (n + 1))

