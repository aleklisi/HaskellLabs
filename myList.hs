data Lista a = EmptyLista | ListaElement {element :: a, next :: Lista a} deriving Show

	
egzampleLista = ListaElement 5 (ListaElement 4 (ListaElement 3 EmptyLista))
egzampleLista2 = ListaElement 'a' (ListaElement 'b' (ListaElement 'c' EmptyLista))
egzampleLista3 = ListaElement 2 (ListaElement 1 (ListaElement 0 EmptyLista))


myMap EmptyLista f = EmptyLista
myMap (ListaElement thisElem next) f = ListaElement (f thisElem) (myMap next f) 

testMyMap = myMap egzampleLista (+10)

myFoldr EmptyLista f neutralElementOfF = neutralElementOfF
myFoldr (ListaElement thisElem next) f neutralElementOfF = f thisElem (myFoldr next f neutralElementOfF)

myFoldl EmptyLista f neutralElementOfF = neutralElementOfF
myFoldl (ListaElement thisElem next) f neutralElementOfF = myFoldl next f (f neutralElementOfF thisElem)

testMyDFoldl 1 = myFoldl egzampleLista (+) 0
testMyDFoldl 2 = myFoldl egzampleLista (*) 1
testMyDFoldr 1 = myFoldr egzampleLista (+) 0
testMyDFoldr 2 = myFoldr egzampleLista (*) 1  

myZipWith EmptyLista EmptyLista = EmptyLista
myZipWith EmptyLista _ = error "Lista are of diffrent length"
myZipWith _ EmptyLista = error "Lista are of diffrent length"
myZipWith (ListaElement a next) (ListaElement b next2) = ListaElement (a,b) (myZipWith next next2)

testMyZipWith = myZipWith egzampleLista egzampleLista2

myConcat EmptyLista EmptyLista = EmptyLista
myConcat EmptyLista a = a
myConcat a EmptyLista = a
myConcat (ListaElement a nexta) (ListaElement b nextb) = ListaElement a (myConcat nexta (ListaElement b nextb))

testMyConcat = myConcat egzampleLista egzampleLista3

fromNormalLista [] = EmptyLista
fromNormalLista (h:t) = ListaElement h (fromNormalLista t)

testFromNormalLista = fromNormalLista [1..5]
testFromNormalLista2 = fromNormalLista []

toNormalLista EmptyLista = []
toNormalLista (ListaElement a next) = a : (toNormalLista next)

testToNormalLista = toNormalLista EmptyLista
testToNormalLista2 = toNormalLista egzampleLista

{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b

fmap id      = id
fmap (p . q) = (fmap p) . (fmap q)
patrz:
https://en.wikibooks.org/wiki/Haskell/The_Functor_class

instance Functor Lista where
	fmap = myMap

-}
	
{-
instance Applicative Lista where
    pure                  = ListaElement
    (ListaElement f) <*> (ListaElement x) = ListaElement (f x)
    _        <*> _        = EmptyLista
-}