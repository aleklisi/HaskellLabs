-- Eq == Equality (== or !=)
data Color = Empty | Black | White deriving (Eq)

instance Show Color where
	show Empty = "_"
	show Black = "X"
	show White = "O"

data Field = Field{field::Color, x::Int, y:: Int} deriving (Eq)

data Board = Board [Field]

instance Show Board where
	show (Board myBoard) = showBoard (Board myBoard)

--instance Read Board where
--	readsPrec _ Board = Board (stringToList move 0 0)

charToColor :: [Char] -> Color
charToColor "_" = Empty
charToColor "X" = Black
charToColor "O" = White
charToColor _ = error "undefined sign"	

testStr = "X_\n_O"

stringToList [] _ _= []
stringToList str x y = case (head str) of
	'_' -> [(Field Empty x y)] ++ (stringToList (tail str) (x + 1) y)
	'X' -> [(Field Black x y)] ++ (stringToList (tail str) (x + 1) y)
	'O' -> [(Field White x y)] ++ (stringToList (tail str) (x + 1) y)
	'\n' -> (stringToList (tail str) 0 (y + 1))

stringToBoard str = Board (stringToList str 0 0)

showBoard :: Board -> [Char]
showBoard (Board []) = ""	
showBoard (Board ((Field field x y):t)) = if x == 0 
	then "\n" ++ show field ++ " " ++ showBoard (Board t)
	else show field ++ " " ++ showBoard (Board t)
 
initializeBoard :: Int -> Board	
initializeBoard size = Board [(Field Empty x y) | y <- [0..size - 1], x <- [0..size - 1]]

insertBoard :: Board -> Color -> Int -> Int -> Board
insertBoard (Board l) field col row = Board (insertList l field col row)

insertList :: [Field] -> Color -> Int -> Int -> [Field]
insertList [] _ _ _ = [] 
insertList ((Field field x y):t) color col row  = if x == col && y == row
	then (Field color x y):t
	else [(Field field x y)] ++ insertList t color col row
	
data Tree = Nil | Leaf (Board , Int, Int) | Branch (Board, Int, Int) [(Tree)]

instance Show Tree where
	show Nil = "?"
	show (Leaf (Board myBoard, priority, height)) = showBoard (Board myBoard) ++ "\n priority: \n" ++ show priority ++ "\n"
	show (Branch (Board myBoard, priority, height) (h:t)) = showBoard (Board myBoard) ++ "\n priority: \n" ++ show priority ++ "\n" ++ show h ++ show t
	
testTree = Branch (initializeBoard 3, 10, 1) [(Leaf(initializeBoard 2, 20, 2)), Nil]

getEmptyFieldsOfBoard (Board []) = []
getEmptyFieldsOfBoard (Board ((Field field x y):t)) =
	if field == Empty then [(x,y)] ++ getEmptyFieldsOfBoard (Board t)
	else getEmptyFieldsOfBoard (Board t)

--nextMoveList l color = [insertList l color x y | (x,y) <- getEmptyFieldsOfBoard (Board l)]

--nextMoveBoard (Board l) color =  (insertBoard (Board l) color x y) | (x,y) <- getEmptyFieldsOfBoard (Board l)

buildGameTree 0 (Board l) field = Leaf ((Board l), 0, 0)
buildGameTree height (Board l) field = Branch ((Board l), 0, height) [buildGameTree (height - 1) (insertBoard (Board l) (changePlayer field) x y ) (changePlayer field) | (x,y) <- getEmptyFieldsOfBoard (Board l)]

changePlayer color =
	case color of 
	Empty -> Empty
	White -> Black
	Black -> White

--buildStrategy 0 (Board l) field = Leaf ((Board l), 0, 0)
--buildStrategy height (Board l) field =  Branch ((Board l), 0, height) [buildStrategy (height - 1) (insertBoard (Board l) (changePlayer field) x y) (changePlayer field) | (x,y,0) <- getBestChose (Board l) field]

--getBestChose (Board []) _ = []
--getBestChose (Board ((Field field x y):t)) currentColor = 
--	if field == Empty then getBestChose t currentColor
--	else if (x == 7 && y == 7) then [(x,y,15)]
--	else if (x > 4 && x < 12 && y > 4 && y < 12 ) then [(x,y,10)]
--	else [(x,y,1)]

getElement (Board []) _ _ = error "Empty board or index out of range"
getElement (Board ((Field field x y):t)) row col = if (x == row && y == col)
	then field
	else getElement (Board t) row col

getCol (Board []) _ = []
getCol (Board ((Field field x y):t)) row = if x == row && y < 19
	then [field] ++ getCol (Board t) row 
	else getCol (Board t) row 
	
getRow (Board []) _ = []
getRow (Board ((Field field x y):t)) col = if y == col && x < 19
	then [field] ++ getRow (Board t) col 
	else getRow (Board t) col 
	
getSlant (Board []) _ _= []
getSlant (Board ((Field field x y):t)) col row = if y == col && x == row
	then [field] ++ getSlant (Board t) (col+1) (row+1) 
	else getSlant (Board t) (col+1) (row+1) 
	
--fiveInRow (Board ((Field field x y):t)) (Color color) = 
--if field == color then
--	nInRow t (Color color) (n-1)
--	else fiveInRow (Board t) (Color color)

--isWinner (Board ((Field field x y):t)) = 

data Pattern color = Pattern {tab::[color]} deriving (Eq, Show)

pattern1 color = Pattern [color, color, color, color, Empty]
pattern2 color = Pattern [color, color, color, Empty, color]
pattern3 color = Pattern [color, color, Empty, color, color]
pattern4 color = Pattern [color, Empty, color, color, color]
pattern5 color = Pattern [Empty, color, color, color, color]

canWin (Board ((Field field x y):t)) color goal = 
canWin (Board ((Field field x y):t)) color goal = 
	if field == color then canWin t color (goal + 1)
	else canWin t color goal

ratingBoard (Board l) color = if canWin (Board l) then 


patternHasNothing (Pattern []) = True
patternHasNothing (Pattern (h:t)) = if h == Empty then patternHasNothing (Pattern t)
	else False

patternHasOneColor (Pattern []) _ = 0
patternHasOneColor (Pattern (h:t)) color = if h == color then 
	1 + patternHasOneColor (Pattern t) color
	else patternHasOneColor (Pattern t) color

findPatternHorizontal (Board ((Field field x y):t)) (Pattern pattern) =
	if patternHasNothing (Pattern pattern) then 0
	else 1

-- inspectBoard (Board ((Field field x y):t)) = 
-- generateTree (Board l) field = 
