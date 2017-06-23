import System.Timeout
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe
import System.IO.Unsafe

--typ pojedynczego pola planszy z wyswietlaniem
data Field = Empty | Black | White deriving (Eq)

instance Show Field where
      show Empty = "_"
      show White = "X"
      show Black = "O"

--funkcja do zamiany tury/koloru
opositFiled Empty = Empty
opositFiled Black = White
opositFiled White = Black

--inicjalizacja pustej planszy size x size
emptyBoard size = Board [(x,y,Empty) | y <- [1..size], x <- [1..size]]

--typ danych dla ca3ej planszy 
data Board = Board [(Int,Int,Field)] deriving (Eq)

--instance Show Board where
--     show (Board l) = printBoard (Board l)

stringToTestBoard = "OX_\nOX_\n___"        
       
         
testBoard  = insertBoard 2 3 Black (insertBoard 2 2 Black (insertBoard 2 1 White(insertBoard 1 3 Black (insertBoard 1 2 Black (insertBoard 1 1 Black (emptyBoard 10))))))


stringToBoard str = Board (stringToList str 1 1)
        
stringToList [] _ _ = []        
stringToList (h:t) x y = case [h] of
      "\n" -> stringToList t x (y + 1)
      "X" -> [(x,y,White)] ++ (stringToList t (x + 1) y)
      "O" -> [(x,y,Black)] ++ (stringToList t (x + 1) y)
      "_" -> [(x,y,Empty)] ++ (stringToList t (x + 1) y)
      
      _ -> error "unrecognised sign can't read in board"
        
printList [] = ""
printList ((x,y,c):t) = if x == 1 
      then "\n" ++ show c ++ printList t
      else show c ++ printList t

--wstawianie do planszy

insertBoard x y val (Board l) = Board (insertList x y val l)

insertList _ _ _ [] = error "coudlt insert to expected filed"
insertList x y value (h:t) = if h == (x,y,Empty)
      then (x,y,value):t
      else [h] ++ (insertList x y value t)

--lista pozycji Fieldów Empty na planszy
listOfEmptyFieldsOfBoard (Board l) = listOfEmptyFieldsOfList l

listOfEmptyFieldsOfList [] = []
listOfEmptyFieldsOfList ((x,y,val):t) = if val == Empty
      then [(x,y)] ++ listOfEmptyFieldsOfList t
      else listOfEmptyFieldsOfList t
      
possobleNextBoards (Board l) value = [insertBoard x y value (Board l) | (x,y) <- listOfEmptyFieldsOfList l] 
      
data Tree = Leaf Board Int | Brunch Board Int [Tree] --deriving (Show)

getBoardFromTree (Leaf b _) = b
getBoardFromTree (Brunch b _ _) = b

buildGameTree 0 (Board l) field = Leaf (Board l) 0
buildGameTree height (Board l) field = Brunch (Board l) height [buildGameTree (height - 1) (insertBoard x y (opositFiled field) (Board l)) (opositFiled field) | (x,y) <- listOfEmptyFieldsOfList l]

getElementOfBoard :: Int -> Int -> Board -> Field
getElementOfBoard x y (Board l) = getElementOfList x y l

getElementOfList _ _ [] = error "couldnt get required element probably index out of bounds"
getElementOfList x y ((col,row,val):t) = if x == col && y == row 
      then val
      else getElementOfList x y t

--ocena planszy...

getRowOfFieldsFromBoard (Board l) rowNr = getRowOfFieldsFromList l rowNr
getColOfFieldsFromBoard (Board l) colNr = getColOfFieldsFromList l colNr

getRowOfFieldsFromList [] _ = []
getRowOfFieldsFromList ((x,y,val):t) rowNr = if  (x,y,val) == (x,rowNr,val)
      then [val] ++ getRowOfFieldsFromList t rowNr
      else getRowOfFieldsFromList t rowNr

getColOfFieldsFromList [] _ = []
getColOfFieldsFromList ((x,y,val):t) colNr = if  (x,y,val) == (x,colNr,val)
      then [val] ++ getColOfFieldsFromList t colNr
      else getColOfFieldsFromList t colNr
               
                     
--pozykiwanie wierszy,kolumn,...

getRowsOfBoard (Board l) boardSize = [getRowOfList l n | n <- [1..boardSize]]-- [getDiagonalsOfListA l (a,0) | a <- [0 .. boardSize+1]] -- ++ [getDiagonalsOfListA l (1,a) | a <- [1..boardSize]]
getColsOfBoard (Board l) boardSize = [getColOfList l n | n <- [1..boardSize]]
getDiagsOfBoard (Board l) boardSize = [getDiagonalsOfListB l (boardSize,r) | r <- [2..boardSize]] ++ [getDiagonalsOfListA l (r,1) | r <- [1..boardSize]] ++ [getDiagonalsOfListA l (1,r) | r <- [2..boardSize]] ++ [getDiagonalsOfListB l (boardSize,r) | r <- [1..boardSize]]

getAllLinesOfBoard b s = (getRowsOfBoard b s) ++ (getColsOfBoard b s) ++ (getDiagsOfBoard b s)

getRowOfList [] _ = []
getRowOfList ((x,y,field):t) n = if x == n
            then [field] ++ (getRowOfList t n)
            else (getRowOfList t n)
            
getColOfList [] _ = []
getColOfList ((x,y,field):t) n = if y == n
            then [field] ++ (getColOfList t n)
            else (getColOfList t n)
            
getDiagonalsOfListA [] _ = []
getDiagonalsOfListA ((x,y,field):t) (r,c) = if (r,c) == (x,y)
            then [field] ++ (getDiagonalsOfListA t (r + 1 ,c + 1))
            else (getDiagonalsOfListA t (r,c))
            
getDiagonalsOfListB [] _ = []
getDiagonalsOfListB ((x,y,field):t) (r,c) = if (r,c) == (x,y)
            then [field] ++ (getDiagonalsOfListB t (r - 1 ,c + 1))
            else (getDiagonalsOfListB t (r,c))
            
myHope 0 col = [Empty,Empty,col,Empty,Empty]
myHope 1 col = [Empty,Empty,Empty,col,Empty]
myHope 2 col = [Empty,col,Empty,Empty,Empty]
myHope 3 col = [Empty,Empty,Empty,Empty,col]
myHope 4 col = [col,Empty,Empty,Empty,Empty]

myHope 5 col = [col,col,Empty,Empty,Empty]
myHope 6 col = [Empty,col,col,Empty,Empty]
myHope 7 col = [Empty,Empty,col,col,Empty]
myHope 8 col = [Empty,Empty,Empty,col,col]

myHope 9 col = [col,col,col,Empty,Empty]
myHope 10 col = [Empty,col,col,col,Empty]
myHope 11 col = [Empty,Empty,col,col,col]

myHope 12 col = [Empty,col,col,col,col]
myHope 13 col = [col,col,col,col,Empty]

myHope 14 col = [Empty,col,col,col,col,Empty]

myHope 15 col = [col,col,col,col,col]

myHope _ col = []

myHopesWeights = [3, 2, 2, 1, 1, 10, 10, 10, 10, 50, 60, 50, 150, 150, 1000, 10000, 0] 

getBoardSize (Board l) = getBListSize l

getBListSize [] = error "worng list structure";
getBListSize ((x1,_,_):(x2,y,f):t) = if x1 < x2
            then 1  + (getBListSize ((x2,y,f):t))
            else 1
getBListSize (h:t) = error "wrong list structure"

markBoard (Board l) color = sum [(myHopesWeights !! i) * (findmyHope (myHope i color) (getAllLinesOfBoard (Board l) (getBoardSize (Board l)))) | i <- [0..15]]

finalBoardMark (Board l) = (markBoard (Board l) White) - (markBoard (Board l) Black)
--findNexMove (Board l) color = do
      --      treeWithHeigths <- buildGameTree 2 (Board l) color
            
                  
myHopes col = [myHope i col | i <- [0..15]]

findmyHope _ [] = 0
findmyHope pat lines =  if (listInList pat (head lines))
            then 1 + (findmyHope pat (tail lines))
            else (findmyHope pat (tail lines))

fp = findmyHope (myHope 2 White) (getAllLinesOfBoard testBoard 3)

markLeafs [] = []
markLeafs ((Leaf (Board l) _):t) = [(Leaf (Board l) (finalBoardMark (Board l)))] ++ (markLeafs t)
markLeafs ((Brunch (Board l) _ []):t) = [Brunch (Board l) (finalBoardMark (Board l)) []] ++ (markLeafs t)
markLeafs ((Brunch (Board l) _ li):t) = [Brunch (Board l) 0 (markLeafs li)] ++ (markLeafs t)
       
sumBruches [] = 0
sumBruches ((Leaf (Board l) val):t) = val
sumBruches ((Brunch (Board l) val []):t) = (sumBruches t)  
sumBruches ((Brunch (Board l) val li):t) = (sumBruches t) + (sumBruches li)

markBruches [] = [] 
markBruches ((Leaf (Board l) val):t) = (Leaf (Board l) val) : (markBruches t)
markBruches ((Brunch (Board l) val []):t) = (Brunch (Board l) val []) : (markBruches t)
markBruches ((Brunch (Board l) val li):t) = (Brunch (Board l) (sumBruches li) (markBruches li)) : (markBruches t) 

instance Eq Tree where
    (Leaf _ vl1) == (Leaf _ vl2) = vl1 == vl2
    (Leaf _ vl1) == (Brunch _ vl2 _) = vl1 == vl2
    (Brunch _ vl1 _) == (Leaf _ vl2) = vl1 == vl2
    (Brunch _ vl1 _) == (Brunch _ vl2 _) = vl1 == vl2

instance Ord Tree where
    (Leaf _ vl1) `compare` (Leaf _ vl2) = vl2 `compare` vl1
    (Leaf _ vl1) `compare` (Brunch _ vl2 _) = vl2 `compare` vl1
    (Brunch _ vl1 _) `compare` (Leaf _ vl2) = vl2 `compare` vl1
    (Brunch _ vl1 _) `compare` (Brunch _ vl2 _) = vl2 `compare` vl1
    
problem = treeLevel ( markBruches ( markLeafs [buildGameTree 2 testBoard White]))

getNextBoardWithBestMove (Board l) treeDepth color = fst ( treeLevel ( markBruches ( markLeafs [buildGameTree treeDepth (Board l) color])) color)

treeLevel ((Brunch (Board l) val li):t) color = if color == White
    then (getBoardFromTree (maximum li), val)
    else (getBoardFromTree (minimum li), val)
getMarkAndBoard ((Leaf (Board l) val):t) = [((Board l),val)] ++ (getMarkAndBoard t)
getMarkAndBoard ((Brunch (Board l) val _):t) = [((Board l),val)] ++ (getMarkAndBoard t)
getMarkAndBoard [] = []

treeLevel2 ((Brunch (Board l) val li):t) = li

canInsertToBoard (Board l) x y = canInsertToList l x y

canInsertToList [] _ _ = False
canInsertToList ((x,y,f):t) cx cy = if x == cx && y == cy && f == Empty
        then True
        else canInsertToList t cx cy

ifWinner (Board l) color = if findmyHope (myHope 15 color) (getAllLinesOfBoard (Board l) (getBoardSize (Board l))) > 0
        then True
        else False

        
printBoard (Board l) = putStrLn $ printList l 
startGameWithPlayer = pvp (emptyBoard 19) Black

pvp (Board l) color =
        do
        printBoard (Board l)        
        c <- readMyInt 
        let x = read c :: Int
        r <- readMyInt 
        let y = read r :: Int
        if canInsertToBoard (Board l) x y 
                then do
                        let newBoard = (insertBoard x y color (Board l))
                        if ifWinner newBoard color
                                then putStrLn (show color ++ " wins the game")
                                else pvp newBoard (opositFiled color)
                else do
                        putStr "worng move play again"
                        pvp (Board l) color

readMyInt :: IO String
readMyInt = do
  c <- getLine
  if isNumber c
    then return c
    else do 
      putStrLn "Please enter a valid position."
      readMyInt

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

startGameWithComputerPlayingWhite = pvc (emptyBoard 19) Black White
startGameWithComputerPlayingBlack = pvc (emptyBoard 19) Black Black

pvc (Board l) currentColor computerColor = do
    printBoard (Board l)
    if currentColor == computerColor 
        then do
            let newBoard = getNextBoardWithBestMove (Board l) 2 $ opositFiled currentColor
            if (ifWinner newBoard currentColor)
                then do putStrLn (show currentColor ++ "wins the game")
                else do pvc newBoard (opositFiled currentColor) computerColor 
        else do
            c <- readMyInt 
            let x = read c :: Int
            r <- readMyInt 
            let y = read r :: Int
            if canInsertToBoard (Board l) x y 
                then do
                    let newBoard = (insertBoard x y currentColor (Board l))
                    if ifWinner newBoard currentColor
                        then do putStrLn ( show currentColor ++ "wins the game")
                        else do pvc newBoard (opositFiled currentColor) computerColor
                else do
                    putStrLn "worng move play again"
                    pvc (Board l) currentColor computerColor

areEqualForLists [] [] = True
areEqualForLists [] _ = False
areEqualForLists _ [] = False
areEqualForLists (ha:ta) (hb:tb) = ha == hb && areEqualForLists ta tb

listInList [] _ = True
listInList _ [] = False
listInList inner outer = areEqualForLists inner (take (length inner) outer) || listInList inner (tail outer)

----------------------------------------------------------------------------------------------------------------------

outOfMaybe (Just a) = a
outOfMaybe Nothing = error "cant get Value of nothing"

--poporaw 6 na 8 na koniec to bd 5 minut
bestBoardTimeOut (Board l) height currentColor = do
    let pom = unsafePerformIO $ timeout timeLimit $ return $! getNextBoardWithBestMove (Board l) 2 (opositFiled currentColor)
    if isNothing pom
        then pom
        else Just $ insertToFirstEmptyFieldOfBoard  currentColor (Board l)
insertToFirstEmptyFieldOfBoard  currentColor (Board l) = insertBoard x y currentColor (Board l) where
   (x,y) = head $ listOfEmptyFieldsOfBoard (Board l)

-------------------------------------------------------------------------------------------------------------------------
timeLimit = (3 * (10 ^ 8))

startGameWithComputerPlayingWhiteWithLimitedTimeForPC = pvcTimeout (emptyBoard 19) Black White
startGameWithComputerPlayingBlackWithLimitedTimeForPC = pvcTimeout (emptyBoard 19) Black Black


pcTurn (Board l) currentColor computerColor = do
    return $ getNextBoardWithBestMove (Board l) 2 $ opositFiled currentColor 

playersTurn (Board l) currentColor computerColor = 
    do
            c <- readMyInt 
            let x = read c :: Int
            r <- readMyInt 
            let y = read r :: Int
            if canInsertToBoard (Board l) x y 
                then do
                    let newBoard = (insertBoard x y currentColor (Board l))
                    if ifWinner newBoard currentColor
                        then do 
                            putStrLn ( show currentColor ++ " wins the game")
                            return newBoard
                        else do pvcTimeout newBoard (opositFiled currentColor) computerColor
                else do
                    putStrLn "worng move play again"
                    pvcTimeout (Board l) currentColor computerColor

--whosTurn :: IO Board -> Field -> Field -> IO Board
whosTurn (Board l) currentColor computerColor = if currentColor == computerColor 
    then unsafePerformIO $ pcTurn (Board l) currentColor computerColor
    else unsafePerformIO $ playersTurn (Board l) currentColor computerColor


pvcTimeout (Board l) currentColor computerColor = do
    printBoard (Board l)
    let newBoard = whosTurn (Board l) currentColor computerColor
    if ifWinner newBoard currentColor
        then do 
            putStrLn ( show currentColor ++ "wins the game")
            return newBoard
        else do pvcTimeout newBoard (opositFiled currentColor) computerColor