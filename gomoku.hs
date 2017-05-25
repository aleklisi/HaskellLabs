data Field = Empty | Black | White deriving (Eq)

instance Show Field where
	show Empty = "_"
	show White = "X"
	show Black = "O"

opositFiled Empty = Empty
opositFiled Black = White
opositFiled White = Black
opositFiled _ = error "Wrong type, Not a Field type"
	
--tworznie pustej planszy
emptyBoard size = Board [(x,y,Empty) | y <- [1..size], x <- [1..size]]

data Board = Board [(Int,Int,Field)]

printBoard 	(Board l) = printList l

instance Show Board where
	show (Board l) = printBoard (Board l)

printList [] = ""
printList ((x,y,c):t) = if x == 1 
	then "\n" ++ show c ++ printList t
	else show c ++ printList t

--wstawianie do planszy

insertBoard x y val (Board l) = Board (insertList x y val l)

insertList _ _ _ [] = []
insertList x y value (h:t) = if h == (x,y,Empty) || h == (x,y,Black) || h == (x,y,White)
	then (x,y,value):t
	else [h] ++ (insertList x y value t)

emptyFieldsOfList [] = []
emptyFieldsOfList ((x,y,val):t) = if val == Empty
	then [(x,y)] ++ emptyFieldsOfList t
	else emptyFieldsOfList t
	
possobleNextBoards (Board l) value = [insertBoard x y value (Board l) | (x,y) <- emptyFieldsOfList l] 
	
data Tree = Leaf Board Int | Brunch Board Int [Tree] deriving (Show)

buildGameTree 0 (Board l) field = Leaf (Board l) 0
buildGameTree height (Board l) field = Brunch (Board l) height [buildGameTree (height - 1) (insertBoard x y (opositFiled field) (Board l)) (opositFiled field) | (x,y) <- emptyFieldsOfList l]


