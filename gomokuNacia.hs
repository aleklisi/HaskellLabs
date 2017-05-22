-- Eq == Equality (== or !=)
data Field = Empty | Black | White deriving (Eq)

instance Show Field where
	show Empty = "_"
	show Black = "X"
	show White = "O"
	
-- (zawartosc, col, row)
data Board = Board [(Field, Int, Int)]

instance Show Board where
	show (Board myBoard) = showBoard (Board myBoard)

showBoard :: Board -> [Char]
showBoard (Board []) = ""	
showBoard (Board ((val,x,y):t)) = if x == 0 
	then "\n" ++ show val ++ " " ++ showBoard (Board t)
	else show val ++ " " ++ showBoard (Board t)

initializeBoard :: Int -> Board	
initializeBoard size = Board [(Empty,x,y) | y <- [0..size - 1], x <- [0..size - 1]]

insertBoard :: Board -> Field -> Int -> Int -> Board
insertBoard (Board l) field col row = Board (insertList l field col row)

insertList :: [(Field, Int, Int)] -> Field -> Int -> Int -> [(Field, Int, Int)]
insertList [] _ _ _ = [] 
insertList ((val,x,y):t) field col row  = if x == col && y == row
	then (field,x,y):t
	else [(val,x,y)] ++ insertList t field col row
	
data Tree = Empty2 | Leaf (Board , Int) | Branch (Board, Int) [(Tree)]

instance Show Tree where
	show Empty2 = "*"
	show (Leaf (Board myBoard, priority)) = showBoard (Board myBoard) ++ "\n priority: \n" ++ show priority ++ "\n"
	show (Branch (Board myBoard, priority) (h:t)) = showBoard (Board myBoard) ++ "\n priority: \n" ++ show priority ++ "\n" ++ show h ++ show t
	
testTree = Branch (initializeBoard 3, 10) [(Leaf(initializeBoard 2, 20)), Empty2]

getEmptyFieldsOfBoard (Board []) = []
getEmptyFieldsOfBoard (Board ((val,x,y):t)) =
	if val == Empty then [(val,x,y)] ++ getEmptyFieldsOfBoard (Board t)
	else getEmptyFieldsOfBoard (Board t)

nextMoveList l field = [insertList l field x y | (_,x,y) <- getEmptyFieldsOfBoard (Board l)]

nextMoveBoard (Board l) field = [Board (insertList l field x y) | (_,x,y) <- getEmptyFieldsOfBoard (Board l)]

changePlayer field =
	case field of 
	Empty -> Empty
	White -> Black
	Black -> White

generateTree (Board l) field = 