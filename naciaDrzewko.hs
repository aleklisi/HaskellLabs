data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

testoweDrzewko = Node 4 (Node 3 Empty Empty) (Node 4 (Node 3 Empty Empty) (Node 1 Empty Empty))
test = Node 1 Empty Empty
-- insert (wstawienie elementu)

-- myInsert x
myInsert :: a -> Tree a -> Tree a
myInsert x (Empty) = Node x Empty Empty
myInsert x (Node y Empty Empty) = Node y (Node x Empty Empty) (Empty)
myInsert x (Node y l r)
     | myDepth l > myDepth r = Node y l (myInsert x r)
     | otherwise         = Node y (myInsert x l) r

myDepth :: Tree a -> Int
myDepth Empty = 0
myDepth (Node _ l r) = 1 + max (myDepth l) (myDepth r)

-- empty (sprawdzanie czy drzewo jest puste)
myEmpty :: Tree a -> Bool
myEmpty Empty = True
myEmpty (Node _ _ _) = False

-- isBinary (sprawdzenie czy drzewo jest drzewem binarnym)

myIsBinary :: Tree a -> Bool
myIsBinary Empty = True
myIsBinary (Node _ _ _) = True

-- search (sprawdzanie czy element jest w drzewie)
mySearch :: Eq t => t -> Tree t -> Bool
mySearch x Empty = False

mySearch x (Node y Empty Empty)
     | (y == x) = True
	 | otherwise = False 

mySearch x (Node y l r) =
     if (y == x) 
	      then True
	      else if (((mySearch x l) == True) || ((mySearch x r) == True))
		       then True
	           else False

-- isBalanced (sprawdzanie czy drzewo jest zrównowa¿one)
myIsBalanced :: Tree a -> Bool
myIsBalanced Empty = True
myIsBalanced (Node _ l r) =
     if ((myDepth l - myDepth r > 1) || (myDepth l - myDepth r < -1))
	      then False
		  else True

-- traverse{VLR, LVR, LRV, VRL, RVL, RLV} - (przejœcie po elementach drzewa 
-- na wszystkie mo¿liwe sposoby, funkcja ma zwracaæ listê wêz³ów)
		  
--first Node second left and then right
myVLR :: Tree t -> [t]
myVLR Empty = []
myVLR (Node a l r) = [a] ++ myVLR l ++ myVLR r

-- first left second Node then right
myLVR :: Tree t -> [t]
myLVR Empty = []
myLVR (Node a l r) = myLVR l ++ [a] ++ myLVR r

-- first left second right after Node
myLRV :: Tree t -> [t]
myLRV Empty = []
myLRV (Node a l r) = myLRV l ++ myLRV r ++ [a]

-- first Node second right after left
myVRL :: Tree t -> [t]
myVRL Empty = []
myVRL (Node a l r) = [a] ++ myVRL r ++ myVRL l

-- first right after Node and then left
myRVL :: Tree t -> [t]
myRVL Empty = []
myRVL (Node a l r) = myRVL r ++ [a] ++ myRVL l

-- first right second left then Node
myRLV :: Tree t -> [t]
myRLV Empty = []
myRLV (Node a l r) = myRLV r ++ myRLV l ++ [a]


-- toString (wypisuj¹c¹ drzewo w postaci „a(b(d,e),c(,f(g,)))” )
myToString :: (Show a) =>  Tree a -> String
myToString (Node x Empty Empty) = show x
myToString (Node x l r) = show x ++ "(" ++ show l ++ " ) (" ++ show r ++ ")"

-- leaves ( zwracaj¹c¹ listê liœci )
myLeaves :: Tree t -> [t]
myLeaves Empty = []
myLeaves (Node a Empty Empty) = [a]
myLeaves (Node a l r) = myLeaves l ++ myLeaves r

-- nnodes (podaj¹c¹ iloœæ wêz³ów) czyli bez lisci
nnodes :: Tree t -> Int
nnodes Empty = 0
nnodes (Node a Empty Empty) = 0
nnodes (Node a l r) = 1 + nnodes l + nnodes r

-- suma wszystkich elementow w drzewku
mySum :: Num t => Tree t -> t
mySum Empty = 0
mySum (Node a l r) = a + mySum l + mySum r

-- nsum (zliczaj¹c¹ sumê wartoœci w wêz³ach)
nsum :: Num t => Tree t -> t
nsum Empty = 0
nsum (Node a Empty Empty) = 0
nsum (Node a l r) = a + nsum l + nsum r

-- tmap (odpowiednik funkcji map dla drzewa)

myMap :: (a -> t) -> Tree a -> Tree t
myMap f Empty = Empty
myMap f (Node a l r) = Node (f a) (myMap f l) (myMap f r)
-- remove (usuwanie elementu)*
{-- 
myRemove x Empty = Empty
myRemove x (Node a left right) =
     if x == a then  myMerge left right
	 else if mySearch x left then myRemove x left
	 else if mySearch x right then myRemove x right
	 else Empty
--}

myRemove2 x Empty = Empty
myRemove2 x (Node a left right) =
     if x == a then  myMerge left right
	 else if mySearch x left then myMerge (myInsert a right) (myRemove2 x left)
	 else if mySearch x right then myMerge (myInsert a left) (myRemove2 x right)
	 else Empty
	 
-- merge (³¹czenie dwóch drzew)
myMerge :: Tree a -> Tree a -> Tree a
myMerge Empty t2 = t2
myMerge (Node a left right) t2 = Node a (myMerge left t2) right

-- getLevel ( zwracaj¹c¹ wêz³y znajduj¹ce siê na wybranym poziomie)
getLevel :: Int -> Tree t -> [t]
getLevel n Empty = []
getLevel 0 (Node a left right) = [a]
getLevel n (Node a left right)
     | n < 0 = []
     | otherwise = (getLevel (n-1) left) ++ (getLevel (n-1) right) 

myInorder :: Tree t -> [t]
myInorder Empty = []
myInorder (Node a left right) = myInorder left ++ [a] ++ myInorder right

getIndex z (x:xs) = 
     if z == x then 1
	 else 1 + getIndex z xs
	 
searchDepthOfNode x Empty = 0
searchDepthOfNode x (Node a left right) = 
     if x == a then 1
	 else if (mySearch x left) == True then 1 + searchDepthOfNode x left
	 else 1 + searchDepthOfNode x right
-- makeLayout (rozmieszczaj¹c¹ wêz³y na p³aszczyŸnie XY, do ka¿dego wêz³a ma byæ przypisama para 
-- (x,y) taka ¿e, x - odpowiada pozycji wêz³a w drzewie (inorder) 
-- a y odpowiada g³êbokoœci na jakiej jest wêze³ )
makeLayout :: (Eq a, Show a) => Tree a -> String
makeLayout Empty = " (Empty) "
makeLayout (Node a left right) = show a ++  " -> ( " ++ show(getIndex a (myInorder (Node a left right))) ++ " , " ++ show (searchDepthOfNode a (Node a left right)) ++ ") ," ++ makeLayout left ++ makeLayout right
-- dumpDOT (wyœwietlaj¹c¹ drzewo w formacie DOT)
{-- digraph G 
  a -> b 
  b -> c
  c-> a
--}
dumpDOT :: (Show a) =>  Tree a -> String
dumpDOT Empty = ""
dumpDOT (Node a Empty Empty) = " " ++ show a ++ " "
dumpDOT (Node a left right) = "Digraph g { " ++ " " ++ show a ++ " -> " ++ show (getLevel 1 (Node a left right))  ++ ", " ++ dumpDOT left ++ ", " ++ dumpDOT right ++ "}"

-- enumerateLevel ( zwraca drzewo gdzie wêz³y s¹ uzupe³nione o poziom, na którym siê znajduj¹)
--data TreeB a level = Empty2 | Node2 a level (TreeB a level) (TreeB a level) deriving (Show)

--enumerateLevel Empty2 level = Empty2
--enumerateLevel (Node2 a level left right) l = Node2 a level (enumerateLevel left (level + 1)) (enumerateLevel right (level + 1))

enumerateLevel Empty _ = Empty
enumerateLevel (Node a left right) level = (Node (a level)) (enumerateLevel left (level+1)) (enumerateLevel right (level+1))