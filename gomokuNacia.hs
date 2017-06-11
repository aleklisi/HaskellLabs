--module Five where
--import System.Random

-- Eq == Equality (== or !=)
data Color = Empty | Black | White deriving (Eq)

instance Show Color where
	show Empty = "_"
	show Black = "X"
	show White = "O"

data Field = Field{field::Color, x::Int, y:: Int} deriving (Eq)

data Board = Board [Field]

--instance Show Board where
--	show (Board myBoard) = showBoard (Board myBoard)

--instance Read Board where
--	readsPrec _ Board = Board (stringToList move 0 0)

--charToColor :: [Char] -> Color
--charToColor "_" = Empty
--charToColor "X" = Black
--charToColor "O" = White
--charToColor _ = error "undefined sign"	

--testStr = "X_\n_O"
{--
stringToList :: [Char] -> Int -> Int -> [Field]
stringToList [] _ _= []
stringToList str x y = case (head str) of
        '_' -> [(Field Empty x y)] ++ (stringToList (tail str) (x + 1) y)
	'X' -> [(Field Black x y)] ++ (stringToList (tail str) (x + 1) y)
	'O' -> [(Field White x y)] ++ (stringToList (tail str) (x + 1) y)
	'\n' -> (stringToList (tail str) 0 (y + 1))
stringToBoard str = Board (stringToList str 0 0)
--}

showBoard2 :: Board -> [Char]
showBoard2 (Board []) = ""	
showBoard2 (Board ((Field field x y):t)) = if x == 1 
	then "\n" ++ show field ++ " " ++ showBoard2 (Board t)
	else show field ++ " " ++ showBoard2 (Board t)

initializeBoard :: Int -> Board	
initializeBoard size = Board [(Field Empty x y) | y <- [1..size], x <- [1..size]]

insertBoard :: Board -> Color -> Int -> Int -> Board
insertBoard (Board l) field col row = Board (insertList l field col row)

insertList :: [Field] -> Color -> Int -> Int -> [Field]
insertList [] _ _ _ = [] 
insertList ((Field field x y):t) color col row  = if x == col && y == row
	then (Field color x y):t
	else [(Field field x y)] ++ insertList t color col row
	
data Tree = Nil | Leaf (Board , Int) | Branch (Board, Int) [(Tree)]

instance Show Tree where
	show Nil = "?"
	show (Leaf (Board myBoard, height)) = showBoard2 (Board myBoard) ++"\n height: \n" ++ show height ++ "\n"
	show (Branch (Board myBoard, height) (h:t)) = showBoard2 (Board myBoard) ++ "\n height: \n" ++ show height ++ "\n" ++ show h ++ show t
	
testTree = Branch (initializeBoard 3, 10) [(Leaf(initializeBoard 3, 10)), Nil]

getEmptyFieldsOfBoard :: Board -> [(Int,Int)]
getEmptyFieldsOfBoard (Board []) = []
getEmptyFieldsOfBoard (Board ((Field field x y):t)) =
	if field == Empty then [(x,y)] ++ getEmptyFieldsOfBoard (Board t)
	else getEmptyFieldsOfBoard (Board t)

generateFirstMove (Board l) color 16 16 = (0, 0)
generateFirstMove (Board l) color row col = if getElement (Board l) row col == Empty then (col, row)
                                            else generateFirstMove (Board l) color (row+1) (col+1)

rateMove (Board []) color _ _ = 0
rateMove (Board t) color x y = 
	if checkFive (Board t) x y color 0 then 5
	else if checkFive (Board t) x y (changePlayer color) 0 then -5
	else if checkFive (Board t) x y color 1 then 4
	else if checkFive (Board t) x y color 2 then 3
	else if checkFive (Board t) x y color 3 then 2
	else if checkFive (Board t) x y color 4 then 1
	else length (getNeighbour (getNeighbourList (Board t) color x y))


--buildGameTree2 :: Int -> Int -> Board -> Color -> Tree
--buildGameTree2 0 priority (Board l) field = Leaf ((Board l), (priority))
--buildGameTree2 counter priority (Board l) field = Branch ((Board l), (priority)) [buildGameTree2 (counter - 1) (rateMove (Board l) field x y) (insertBoard (Board l) (changePlayer field) x y ) (changePlayer field) | (x,y) <- getNeighbour (getNeighbpourList (Board l) field x2 y2) | (x2 y2) <- getEmptyFieldsOfBoard (Board l)]

buildGameTree :: Int -> Board -> Color -> Tree
buildGameTree 0 (Board l) field = Leaf ((Board l), 0)
buildGameTree height (Board l) field = Branch ((Board l), height) [buildGameTree (height-1) (insertBoard (Board l) (changePlayer field) x y ) (changePlayer field) | (x,y) <- getEmptyFieldsOfBoard (Board l)]


--rateBoard (Board l) field x y 0
rateTree [] _ = []
rateTree (Leaf (Board l, h):t) color = [Leaf (Board l, (rateBoard (Board l) color 0 0 0))] ++  rateTree t color
rateTree ((Branch (Board l, h) []):t) color =  [Branch (Board l, (rateBoard (Board l) color 0 0 0)) [] ] ++ rateTree t color
rateTree ((Branch(Board l,h) list) : t) color = [Branch((Board l), (rateBoard (Board l) color 0 0 0)) (rateTree list color)] ++ rateTree t color
--}

sumFromTree [] = 0
sumFromTree (Leaf (_, h):t) = h + sumFromTree t
sumFromTree ((Branch (_, h) []) : t) = h + sumFromTree t
sumFromTree ((Branch(_,h) list) : t) = h + sumFromTree t + sumFromTree list

--getMax [] = 0
--getMax (Leaf (_, h):t) = sumFromTree (Leaf (_, h):t)
--getMax ((Branch (_, h) []) : t) = getMax sumFromTree ((Branch (_, h) []) : t)
--getMax ((Branch(_,h) list) : t) = max sumFromTree 

getBoardFromTree :: Tree -> Board
getBoardFromTree (Leaf ((Board l), _)) = (Board l)
getBoardFromTree (Branch ((Board l), _) _ ) = (Board l)

getNeighbour :: (Eq t, Eq t1, Num t, Num t1) => [(t, t1)] -> [(t, t1)]
getNeighbour [] = []
getNeighbour ((x,y):xs) = 
	if x == 0 && y == 0 then getNeighbour xs
	else [(x,y)] ++ getNeighbour xs
	
getNeighbourList :: Board -> Color -> Int -> Int -> [(Int, Int)]
getNeighbourList (Board l) color col row = 
	[getOneNeighbour (Board l) color (row + 1) (col)] ++ 
	[getOneNeighbour (Board l) color (row - 1) (col)] ++ 
	[getOneNeighbour (Board l) color (row + 1) (col + 1)] ++ 
	[getOneNeighbour (Board l) color (row + 1) (col - 1)] ++ 
	[getOneNeighbour (Board l) color (row - 1) (col - 1)] ++ 
	[getOneNeighbour (Board l) color (row - 1) (col + 1)] ++ 
	[getOneNeighbour (Board l) color (row) (col + 1)] ++ 
	[getOneNeighbour (Board l) color (row) (col - 1)]
	
getOneNeighbour :: Board -> Color -> Int -> Int -> (Int, Int)
getOneNeighbour (Board l) color row col =   
	if (row < 16) && (row > 0) && (col < 16) && (col > 0) then 
		if getElement (Board l) row col == color then (col,row)
		else (0, 0)
	else (0, 0)

changePlayer color =
	case color of 
	Empty -> Empty
	White -> Black
	Black -> White

{--
getMoveComputer :: Board -> Int -> Color -> [(Int,Int)]
getMoveComputer (Board l) 5 color _ = generateFirstMove (Board l) color 6 6
getMoveComputer (Board l) counter color (x,y) = 
    if checkFive (Board l) x y color counter then (x,y)
    else getMoveComputer (Board l) (counter+1) color (x,y)
getMoveComputer (Board l) counter color ((x,y):xs) = 
    if checkFive (Board l) x y color counter then (x,y)
    else getMoveComputer (Board l) (counter) color xs
--buildStrategy 0 (Board l) field = Leaf ((Board l), 0, 0)
--}
--buildStrategy height (Board l) field =  Branch ((Board l), 0, height) [buildStrategy (height - 1) (insertBoard (Board l) (changePlayer field) x y) (changePlayer field) | (x,y,0) <- getBestChose (Board l) field]
--}

getElement :: Board -> Int -> Int -> Color
getElement (Board []) _ _ = error "Empty board or index out of range"
getElement (Board ((Field field x y):t)) row col = if (x == row && y == col)
	then field
	else getElement (Board t) row col

getCol :: Board -> Int -> [Color]
getCol (Board []) _ = []
getCol (Board ((Field field x y):t)) col = if x == col && y < 16
	then [field] ++ getCol (Board t) col 
	else getCol (Board t) col 

getRow :: Board -> Int -> [Color]	
getRow (Board []) _ = []
getRow (Board ((Field field x y):t)) row = if y == row && x < 16
	then [field] ++ getRow (Board t) row 
	else getRow (Board t) row 

getSlant :: Board -> Int -> [Color]	
getSlant (Board []) _ = []
getSlant (Board ((Field field x y):t)) col = if x == col && y == x
	then [field] ++ getSlant (Board t) (col+1)
	else getSlant (Board t) (col)
 
testBoard = insertBoard(insertBoard(insertBoard(insertBoard (insertBoard (initializeBoard 15) Black 1 1) Black 2 1)Black 3 1) Black 4 1) Black 5 1

testBoard2 = insertBoard(insertBoard(insertBoard(insertBoard (insertBoard (initializeBoard 15) Black 1 1) Black 1 2)Black 1 3) Black 1 4) Black 1 5

testBoard3 = insertBoard(insertBoard(insertBoard(insertBoard (insertBoard (initializeBoard 15) Black 1 1) Black 2 2)Black 3 3) Black 4 4) Black 5 5

testBoard4 = insertBoard(insertBoard(insertBoard (insertBoard (initializeBoard 15) Black 1 1) Black 2 1)Black 3 1) Black 5 1

testBoard5 = insertBoard(insertBoard(insertBoard(insertBoard (insertBoard (initializeBoard 15) Black 15  1) Black 15 2)Black 15 15) Black 15 14) Black 15 10

testBoard6 = insertBoard (insertBoard (insertBoard (initializeBoard 15) Black 2 2) Black 3 3) Black 5 5
testBoard7 = insertBoard (insertBoard (insertBoard (initializeBoard 15) Black 2 2) Black 2 3) Black 2 5
testBoard8 = insertBoard (insertBoard (insertBoard (initializeBoard 15) Black 2 2) Black 2 6) Black 2 12
testBoard9 = insertBoard (insertBoard (initializeBoard 15) Black 2 2) Black 2 7

checkFive :: Board -> Int -> Int -> Color -> Int -> Bool
checkFive (Board x) row col color counter
	| searchFiveInLine (getRow (Board x) row) color counter 0 = True  
	| searchFiveInLine (getCol (Board x) col) color counter 0 = True 
	| searchFiveInLine (getSlant (Board x) row) color counter 0 = True 
	| otherwise = False

searchFiveInLine :: (Eq a1, Num a, Ord a) => [a1] -> a1 -> a -> a -> Bool	
searchFiveInLine [] _ _ _ = False 
searchFiveInLine (x:xs) color counter tmp
	| tmp > 4 = False
	| counter >= 5 = True
	| x == color = searchFiveInLine xs color (counter+1) tmp
	| otherwise = searchFiveInLine xs color counter (tmp + 1)

rateBoard :: Board -> Color -> Int -> Int -> Int -> Int
rateBoard (Board m) color 16 16 _ = 0
rateBoard (Board m) color col row counter 
	| checkFive (Board m) col row color counter = 5 - counter
        | checkFive (Board m) col row (changePlayer color) counter = -5 + counter
	| otherwise = rateBoard (Board m) color col row (counter + 1)

data Player = Human Color
            | AI    Color
  deriving (Eq,Show)

data Mode = Single
          | Duo
  deriving (Eq,Show)

-- Print the column number
colMark :: [Color] -> Int -> IO ()
colMark [] _ = putStrLn ""
colMark (x:xs) idx | idx <  10 = putStr ((show idx) ++ "  ") >> colMark (xs) (idx+1) 
                   | idx >= 10 = putStr ((show idx) ++ " " ) >> colMark (xs) (idx+1)

-- Print the row number
rowMark :: Int -> Int -> IO ()
rowMark x y
	| x == 1 && y == 1 = putStr (show x ++ " ")
	| x == 1 && x < 10 = putStrLn "" >> putStr ((show y) ++ " ")
	| x == 1 && x >= 10 = putStrLn "" >> putStr ((show y) ++ "")
        | otherwise  = putStr ""

-- Print the cells
showCell :: Board -> IO ()
showCell (Board []) = putStr ""
showCell (Board((Field field x y):ys)) = 
	rowMark x y >> putStr (" " ++ (show field) ++ " ") >> showCell (Board ys) 


showBoard :: Board -> IO ()
showBoard (Board (x:xs)) = putStr "   " >> colMark (getRow (Board(x:xs)) 1) 1 >> showCell (Board(x:xs)) >> putStr "   " >> putStrLn ""

-- Check if the input stone is valid
isGood :: Board -> Int -> Int -> Bool
isGood (Board x) c r = (c > 0) && (r > 0) && ((getElement (Board x) c r) == Empty )

-- A higher order function for the currentPlayer cellColor nextPlayer function 
helper :: t -> t -> Player -> t
helper a _ ( Human Black ) = a
helper _ b ( Human White) = b
helper a _ ( AI Black ) = a
helper _ b ( AI White) = b

-- Print our the current player
currentPlayer :: Player -> IO()
currentPlayer = helper (putStrLn "BLACK's turn: ") (putStrLn "WHITE's turn: ")

-- Get the color of the stone need to input
cellColor :: Player -> Color
cellColor = helper Black White

-- Decide next turn is which player's
nextPlayer :: Player -> Mode -> Player
nextPlayer (Human o)     Duo    = helper (Human White) (Human Black) (Human o)
nextPlayer (Human Black) Single = helper (AI White) (Human Black) (Human Black)
nextPlayer (Human White) Single = helper (Human White) (AI Black) (Human White)
nextPlayer (AI Black) _ = helper (Human White) (AI Black) (AI Black)
nextPlayer (AI White) _ = helper (AI White) (Human Black) (AI White)

-- Check if the input is valid
isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

-- Get column position from input
readCol :: IO String
readCol = do
  putStr "Col: "
  c <- getLine
  if isNumber c
    then return c
    else do 
      putStrLn "Please enter a valid position."
      readCol

-- Get row position from input
readRow :: IO String
readRow = do
  putStr "Row: "
  c <- getLine
  if isNumber c 
    then return c
    else do
      putStrLn "Please enter a valid position." 
      readRow


-- A function to manage all operations at every single turn 
loopfunc :: Board -> Int -> Int -> Player -> Mode -> IO ()
loopfunc (Board x) col row player m = 
    do
      if isGood (Board x) col row
      then do
        if checkFive (insertBoard (Board x) (cellColor player) col row) row col (cellColor player) 0
         then do
          showBoard (insertBoard (Board x) (cellColor player) col row)
          putStrLn "You Win!!!"
        else
          gameLoop (insertBoard (Board x) (cellColor player) col row) (nextPlayer player m) m
      else do
        print "Bad Position!!! Please input again."
        gameLoop (Board x) player m

-- A loop for generate every game 
gameLoop :: Board -> Player -> Mode -> IO ()
gameLoop (Board x) (AI o) m = 
  do showBoard (Board x)
     currentPlayer (AI o)
     let (col,row) = head (getEmptyFieldsOfBoard (Board x))
     putStr "Col: "
     print col
     putStr "Row: "
     print row
     loopfunc (Board x) col row (AI o) m


gameLoop (Board x) (Human o) m =
  do showBoard (Board x)
     currentPlayer (Human o)
     c <- readCol
     r <- readRow
     let col = read c :: Int
     let row = read r :: Int	 
     loopfunc (Board x) col row (Human o) m
	 
-- Set the game mode according to the input
whichMode :: String -> IO ()
whichMode s
    | s == "1"  = gameLoop (initializeBoard 15) (AI Black)    Single
    | s == "2"  = gameLoop (initializeBoard 15) (Human Black) Single
    | s == "3"  = gameLoop (initializeBoard 15) (Human Black) Duo
    | otherwise = main

-- Game begins here
-- Player choose the game mode
main :: IO ()
main = do
    putStrLn "Please choose one mode."
    putStrLn "1: Play against AI(Black)"
    putStrLn "2: Play against AI(White)"
    putStrLn "3: Play against another player"
    putStr   "Enter the mode number you choose (1/2/3): "
    s <- getLine
    whichMode s
