--zad1

data Tree a = Nil | Node {val::a,l::(Tree a),r::(Tree a)} deriving (Eq,Ord)

instance Show a => Show (Tree a) where
	show Nil = "|"
	show (Node value ltree rtree) = show value ++ "[(" ++ show ltree ++")("++ show rtree++")]"
	
myTree = Node 0 (Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)) (Nil)

--zad2

data STree a = SEmpty | SLeaf a | SBranch a (STree a) (STree a) deriving (Show,Eq)

treeRebuild :: Tree a -> STree a
treeRebuild  Nil = SEmpty
treeRebuild (Node a Nil Nil) = SLeaf a
treeRebuild (Node a l r) = SBranch a (treeRebuild l) (treeRebuild r)

--zad3
{--
Napisz nastêpuj¹ce funkcje u¿ywaj¹c fold{r,l}:
sum
product
reverse
and
or
head
last
--}

--listy testowe:
veryBigList = [1..100]
logicList1 = []
logicList2 = [True,True]
logicList3 = [True,True,False]
logicList4 = [False,False,False]


mySum = foldl (+) 0
myProduct = foldl (*) 1
myReverse = foldl (flip (:)) []
myAnd = foldl (&&) True
myOr = foldl (||) False
myHead = foldr (\a b->a) undefined
myLast = foldl (\a b->b) undefined

--testy

testSum = mySum veryBigList

testProduct = myProduct veryBigList

testReverse = myReverse veryBigList

testAnd1 = myAnd logicList1
testAnd2 = myAnd logicList2
testAnd3 = myAnd logicList3
testAnd4 = myAnd logicList4

testOr1 = myOr logicList1
testOr2 = myOr logicList2
testOr3 = myOr logicList3
testOr4 = myOr logicList4

testHead = myHead veryBigList

testLast = myLast veryBigList