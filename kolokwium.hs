-- Napisz funkcje generujaca drzewo o zadanej glebokosci i rozgalezzieniu w kazdym z wezlow.
-- Wartosc w wezle ma byc takA sama i podawana jako argument funkcji. 
-- Zaproponuj typ dla drzewa, ktore spelni powyzsze wymagania. Napisz klase Funktor dla tego typu.

data Tree a = Empty | Node a [Tree a] deriving Show

buildTree :: (Num a1, Eq a1) => a1 -> a -> Tree a
buildTree 0 a = Empty
buildTree 1 a = Node a [Empty]
buildTree height a = Node a [buildTree (height - 1) a]

instance Functor Tree where
	fmap _ Empty = Empty
	fmap f (Node a [left]) = Node (f a) [fmap f left]
	
data TreeB a = EmptyB | Leaf a | N (TreeB a) a (TreeB a) deriving Show

buildTreeB 0 a = EmptyB
buildTreeB 1 a = Leaf a
buildTreeB height a = N (buildTreeB (height - 1) a) a (buildTreeB (height - 1) a)

instance Functor TreeB where
		fmap _ EmptyB = EmptyB
		fmap f (Leaf a) = Leaf (f a)
		fmap f (N left a right) = N (fmap f left) (f a) (fmap f right)
-- Stworz wlasny typ implementujacy liste jednokierunkowa. Dopisz dla tego typu nastepujace funkcje:
data List a = Null | ListElement {element :: a, next :: List a} deriving (Show, Read, Eq, Ord)  

testList = ListElement 5 (ListElement 4 (ListElement 3 Null))
testList2 = ListElement 'a' (ListElement 'b' (ListElement 'c' Null))
testList3 = ListElement 4 (ListElement 1 (ListElement 0 Null))
testList4 = ListElement (*2) (ListElement (+3) (ListElement (^2) Null))
-- map
instance Functor List where
	fmap _ Null = Null
	fmap f (ListElement a next) = ListElement (f a) (fmap f next)

testMyMap = fmap (+10) testList 
-- foldr (tworzy kombinacje struktury qykorzystujac funkcj podana jako argument pierwszy uzywajac argumentu drugiego jako parametru poczatkowego)
myFoldr f neutralElement Null = neutralElement
myFoldr f neutralElement (ListElement elem next) = f elem (myFoldr f neutralElement next)

-- foldl-
myFoldl f neutralElement Null = neutralElement
myFoldl f neutralElement (ListElement elem next) = myFoldl f ( f neutralElement elem) next 

--test do fold
testFoldl1 = myFoldl (+) 0 testList 
testFoldr1 = myFoldr (+) 0 testList 
testFoldl2 = myFoldl (*) 1 testList 
testFoldr2 = myFoldr (*) 1 testList 
-- zipWith 
myZipWith Null Null = Null
myZipWith Null _ = error "Lists are of different length"
myZipWith _ Null = error "Lists are of different length"
myZipWith (ListElement a next) (ListElement b next2) = ListElement (a,b) (myZipWith next next2)
-- operator analogiczny do ++ dla zwyklej listy nazwa dowolna

infixr 5 .++
(.++) :: List a -> List a -> List a
Null .++ Null = Null
Null .++ a = a
a .++ Null = a
(ListElement a nexta) .++ (ListElement b next2) = ListElement a (nexta .++ (ListElement b next2))


-- fromNormalList (konwersja z isty wbudowanej do zaproponowanego typu)
fromNormalList :: [a] -> List a
fromNormalList [] = Null
fromNormalList (x:xs) = ListElement x (fromNormalList xs)


-- toNormalList (konwersja z zaproponowanego typu do listy wbudowanej)
toNormalList :: List a -> [a]
toNormalList Null = []
toNormalList (ListElement a next) = a : toNormalList next
-- Napisz dla tego typu instancje klasyy Functor i Applicative

--instance Applicative List where
{-
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  
	-}
	
	
instance Applicative List where
		pure x = ListElement x Null
		fs <*> xs = fromNormalList  ([f x | f <- (toNormalList fs), x <- (toNormalList xs)])
	
	
	
	
	-- fu wyliczaja wartosc wielomianu
wielomian coeff x = sum $ zipWith (*) coeff $ map (x^) [0..] 
	
	
wielomian2 [] x = 0
wielomian2 l x = (head l) * (x^((length l) - 1)) + ( wielomian2 (tail l) x)
	
	
f .$ a = f a

(f .? g) a =  f $ g a

odwrocNapis [] = []
odwrocNapis (h:t) = (odwrocNapis t) ++ [h]

main = do
	x <- getLine
	putStrLn $ odwrocNapis x
	
--data Id a	 = Id a

--instance Monad Id where
  --  return a = Id a
   -- (Id a) >>= f = f a
	
(.=<<) :: Monad m => (a -> m b) ->  m a -> m b
f .=<< x = x >>= f

data Tree2 a = E2 |L2 a | N2 (Tree2 a) a (Tree2 a)

testTree2 = N2 (N2 (L2 1) 2 (L2 2)) 0 (L2 3)

sumNodes :: Tree2 t -> Int
sumNodes E2 = 0
sumNodes (L2 _) = 1
sumNodes (N2 left _ right) = 1 + sumNodes left + sumNodes right

preOrder E2 = []
preOrder (L2 a) = [a]
preOrder ((N2 left a right)) = a : (preOrder left) ++ (preOrder right) 

inOrder E2 = []
inOrder (L2 a) = [a]
inOrder ((N2 left a right)) = (inOrder left) ++ [a] ++ (inOrder right) 

postOrder E2 = []
postOrder (L2 a) = [a]
postOrder ((N2 left a right)) = (postOrder left) ++ (postOrder right) ++ [a] 

main2 = do { putStrLn "Czesc" ; x<-getLine ; putStr x ; putChar '\n' } 

main3 = putStrLn "Czesc" >> getLine >>= putStr  >> putChar '\n' 

instance Functor [] where
	fmap = map
	
instance Functor [] where
	fmap _ [] = []
	fmap f (h:t) = (f h) : (fmap f t)

data Maybe a = Nothing | Just a

instance Monad Maybe where
	return x = Just x
	(Just a) >>= f = Just (f a)
	Nothing >>= f = Nothing
	
	
	_ >> x = Just x
