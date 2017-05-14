data Tree a = Nil | Node {val::a,l::(Tree a),r::(Tree a)} deriving (Show,Eq,Ord)

myTree1 = Nil
myTree2 = Node 1 (Node 2 (Node 4 Nil Nil) (Nil)) (Node 3 (Nil) (Nil))
myTree3 = Node 1 (Node 2 (Node 4 (Node 5 Nil (Node 6 (Node 7 Nil Nil) Nil)) (Nil)) (Nil)) (Nil)

treeInsert :: Ord a => Tree a -> a -> Tree a
treeInsert Nil ele = Node ele Nil Nil
treeInsert tree ele = if treeDepth (l tree) > treeDepth (r tree)then
     Node (val tree) (treeInsert (l tree) ele) (r tree) else Node (val tree) (l tree) (treeInsert (r tree) ele)

treeDepth :: Tree a -> Int
treeDepth Nil = 0
treeDepth tree = max (treeDepth (l tree)) (treeDepth (r tree)) + 1

treeIsEmpty :: Tree a -> Bool
treeIsEmpty Nil = True
treeIsEmpty _ = False

treeSearch :: Eq a => Tree a -> a -> Bool
treeSearch Nil _ = False
treeSearch tree ele = (val tree) == ele || treeSearch (l tree) ele || treeSearch (r tree) ele

modul :: Int -> Int
modul arg = if arg > 0 then arg else (-arg) 

treeIsBalanced :: Tree a -> Bool
treeIsBalanced Nil = True
treeIsBalanced tree = 1 >= (modul ((treeDepth (l tree)) - (treeDepth (r tree)))) && treeIsBalanced (l tree) && treeIsBalanced (r tree)


treeVLR :: Tree a -> [a]
treeVLR Nil = []
treeVLR tree = [(val tree)] ++ (treeVLR (l tree)) ++ (treeVLR (r tree))
treeLVR tree = (treeVLR (l tree)) ++ [(val tree)] ++ (treeVLR (r tree))
treeLRV tree = (treeVLR (l tree)) ++ (treeVLR (r tree)) ++ [(val tree)]
treeVRL tree = [(val tree)] ++ (treeVLR (r tree)) ++ (treeVLR (l tree))
treeRVL tree = (treeVLR (r tree)) ++ [(val tree)] ++ (treeVLR (l tree))
treeRLV tree = (treeVLR (r tree)) ++ (treeVLR (l tree)) ++ [(val tree)]

toString :: Show a => Tree a -> [Char]
toString Nil = "()"
toString tree = (show (val tree)) ++ "(" ++ (toString (l tree)) ++ ")" ++ "(" ++ (toString (r tree)) ++ ")"

treeLeaves :: Tree a -> [a]
treeLeaves Nil = []
treeLeaves (Node arg Nil Nil) = [arg] 
treeLeaves tree = treeLeaves (l tree) ++ treeLeaves (r tree)

treeNNodes :: Tree a -> Int
treeNNodes Nil = 0
treeNNodes tree = 1 + (treeNNodes (l tree)) + (treeNNodes (r tree))

treeSum :: Num a => Tree a -> a
treeSum Nil = 0
treeSum tree = (val tree) + (treeSum (l tree)) + (treeSum (r tree))

treeMap :: Num a => Tree a -> (a -> b) -> Tree b
treeMap Nil f = Nil
treeMap tree f = Node (f (val tree)) (treeMap (l tree) f) (treeMap (r tree) f)

treeRemove :: Eq a => Tree a -> a -> Tree a
treeRemove Nil _ = Nil
treeRemove tree ele = if (val tree) == ele 
     then treeMerge (treeRemove (l tree) ele) (treeRemove (r tree) ele) 
     else Node (val tree) (treeRemove (l tree) ele) (treeRemove (r tree) ele)  

treeMerge :: Tree a -> Tree a -> Tree a
treeMerge Nil a = a
treeMerge treea treeb = Node (val treea) (treeMerge (l treea) treeb) (r treea)







 
