data Field = Empty | Black | White deriving (Eq)

instance Show Field where
	show Empty = "*"
	show White = "X"
	show Black = "O"

--tworznie pustej planszy
board size = [(x,y,Empty) | y <- [1..size], x <- [1..size]]

--wyświetlanie planszy	
printBoard [] = ""
printBoard ((x,y,c):t) = if x == 1 
	then "\n" ++ show c ++ printBoard t
	else show c ++ printBoard t
	
--wstawianie do planszy
insertBoard _ _ _ [] = []
insertBoard x y value (h:t) = if h == (x,y,Empty) || h == (x,y,Black) || h == (x,y,White)
	then (x,y,value):t
	else [h] ++ (insertBoard x y value t)
	
data Tree a = Tree a [Tree a] | Nil deriving Show

