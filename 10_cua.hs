data Queue a = Queue [a] [a]
     deriving (Show)

instance Eq a => Eq (Queue a) where 
	(Queue [] l) == (Queue [] g) = l == g
	(Queue l []) == (Queue g []) = l == g
	(Queue l g) == (Queue a b) = l ++ (reverse g) == a ++ (reverse b) 

create :: Queue a
create = Queue [] [] 

push :: a -> Queue a -> Queue a
push n (Queue a b) = Queue a (n:b) 

pop :: Queue a -> Queue a
pop (Queue (_:ys) l) = (Queue ys l) 
pop (Queue [] a) = (Queue ys [])
	where (_:ys) = reverse a	

top :: Queue a -> a
top (Queue (y:_) _) = y
top (Queue [] g) = y
	where (y:_) = reverse g

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False