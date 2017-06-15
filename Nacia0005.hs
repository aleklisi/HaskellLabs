-- Eq == Equality (== or !=)
data Color = Empty | Black | White deriving (Eq)

instance Show Color where
	show Empty = "_"
	show Black = "X"
	show White = "O"

data Field = Field{field::Color, x::Int, y:: Int} deriving (Eq)

data Board = Board [Field]

instance Show Board where
	show (Board myBoard) = showBoard2 (Board myBoard)

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

buildGameTree :: Int -> Board -> Color -> Tree
buildGameTree 0 (Board l) field = Leaf ((Board l), 0)
buildGameTree height (Board l) field = Branch ((Board l), height) [buildGameTree (height-1) (insertBoard (Board l) (changePlayer field) x y ) (changePlayer field) | (x,y) <- getEmptyFieldsOfBoard (Board l)]

firstRewriteTree [] _ = []
firstRewriteTree (Leaf (Board l, h):t) color = (Leaf (Board l, (rateBoard (Board l) color)):(firstRewriteTree t color))
firstRewriteTree ((Branch (Board l, h) []):t) color = ((Branch (Board l, (rateBoard (Board l) color)) []):(firstRewriteTree t color))
firstRewriteTree ((Branch(Board l,h) list) : t) color = ((Branch(Board l,h) (firstRewriteTree list color)) : (firstRewriteTree t color)) 


secondRewriteTree [] = []
secondRewriteTree (Leaf (Board l, h):t) = Leaf (Board l, h):(secondRewriteTree t)
secondRewriteTree ((Branch (Board l, h) []):t) = ((Branch (Board l, h) []):(secondRewriteTree t))
secondRewriteTree ((Branch(Board l,h) list) : t) = ((Branch(Board l,(sumFromTree list)) (secondRewriteTree list)) : (secondRewriteTree t))

ratedTree height currentBoard currentColor = secondRewriteTree $ firstRewriteTree [(buildGameTree height currentBoard currentColor)] currentColor

getSubNodes [(Branch(Board l,h) list)] = list

getBestSubTree [] = error "list cant be empty - no more possoble moves"
getBestSubTree list = maximum list

getBoardFromBestNodeWithItsMark (Branch (a,b) _) = (a,b)
getBoardFromBestNodeWithItsMark _ = error "cant find best board"

getBoardFromBestNode (Branch (a,_) _) = a

getBestRatedBoard height currentBoard currentColor = getBoardFromBestNode $ getBestSubTree $ getSubNodes $ ratedTree height currentBoard currentColor

instance Eq (Tree) where
	(Leaf (_,x)) == (Leaf (_,y)) = x == y
	Branch(_,x) _ == Branch(_,y) _ = x == y
	Branch(_,y) _ == Leaf(_,x) = x == y
	Leaf(_,x) == Branch(_,y) _ = x == y
	
instance Ord (Tree) where
	(Leaf (_,x)) `compare` (Leaf (_,y)) = x `compare` y
	Branch(_,x) _ `compare` Branch(_,y) _ = x `compare` y
	Branch(_,y) _ `compare` Leaf(_,x) = x `compare` y
	Leaf(_,x) `compare` Branch(_,y) _ = x `compare` y	

getDifretFieldBoards (Board a) (Board b) = getDifretFieldLists a b

getDifretFieldLists [] _ = error ""
getDifretFieldLists _ [] = error ""

getDifretFieldLists ((Field c1 x1 y1):t1) ((Field c2 x2 y2):t2) = if c1 == c2 
	then getDifretFieldLists t1 t2
	else (x1,y1)

getNextMoveFromNextBestBoard height currentBoard currentColor = getDifretFieldBoards (getBestRatedBoard height currentBoard currentColor) currentBoard
	
sumFromTree [] = 0
sumFromTree (Leaf (_, h):t) = h + sumFromTree t
sumFromTree ((Branch (_, h) []) : t) = h + sumFromTree t
sumFromTree ((Branch(_,h) list) : t) = sumFromTree list + sumFromTree t

changePlayer color =
	case color of 
	Empty -> Empty
	White -> Black
	Black -> White

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

rateBoard :: Board -> Color -> Int
rateBoard (Board x) color = goRateThisShit (Board x) color (getSize (Board x)) (getSize (Board x))

intSquareRoot :: Int -> Int
intSquareRoot n = try n where
  try i   | i*i > n   = try (i - 1) 
          | i*i <= n  = i

getSize :: Board -> Int
getSize (Board []) = 0
getSize (Board x) = intSquareRoot (getSizeAll (Board x))

getSizeAll (Board []) = 0
getSizeAll (Board ((Field field x y):t)) = 1 + getSizeAll (Board t)

goRateThisShit (Board []) _ _ _ = 0
goRateThisShit (Board x) color col row = 
	if (col <= getSize (Board x)) && (row <= getSize (Board x)) && col > 0 && row > 0 then
		if (checkFive (Board x) row col color 0) then 1
		else if (checkFive (Board x) row col (changePlayer color) 0) then -1
		else goRateThisShit (Board x) color (row - 1) (col - 1)
	else 0
	

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
     let (col,row) = getNextMoveFromNextBestBoard 2 (Board x) o
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
