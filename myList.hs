data List a = EmptyList | ListElement {element :: a, next :: List a} deriving Show

	
egzampleList = ListElement 5 (ListElement 4 (ListElement 3 EmptyList))
egzampleList2 = ListElement 'a' (ListElement 'b' (ListElement 'c' EmptyList))
egzampleList3 = ListElement 2 (ListElement 1 (ListElement 0 EmptyList))


myMap EmptyList f = EmptyList
myMap (ListElement thisElem next) f = ListElement (f thisElem) (myMap next f) 

testMyMap = myMap egzampleList (+10)

myFoldr EmptyList f neutralElementOfF = neutralElementOfF
myFoldr (ListElement thisElem next) f neutralElementOfF = f thisElem (myFoldr next f neutralElementOfF)

myFoldl EmptyList f neutralElementOfF = neutralElementOfF
myFoldl (ListElement thisElem next) f neutralElementOfF = myFoldl next f (f neutralElementOfF thisElem)

testMyDFoldl 1 = myFoldl egzampleList (+) 0
testMyDFoldl 2 = myFoldl egzampleList (*) 1
testMyDFoldr 1 = myFoldr egzampleList (+) 0
testMyDFoldr 2 = myFoldr egzampleList (*) 1  

myZipWith EmptyList EmptyList = EmptyList
myZipWith EmptyList _ = error "list are of diffrent length"
myZipWith _ EmptyList = error "list are of diffrent length"
myZipWith (ListElement a next) (ListElement b next2) = ListElement (a,b) (myZipWith next next2)

testMyZipWith = myZipWith egzampleList egzampleList2

myConcat EmptyList EmptyList = EmptyList
myConcat EmptyList a = a
myConcat a EmptyList = a
myConcat (ListElement a nexta) (ListElement b nextb) = ListElement a (myConcat nexta (ListElement b nextb))

testMyConcat = myConcat egzampleList egzampleList3

fromNormalList [] = EmptyList
fromNormalList (h:t) = ListElement h (fromNormalList t)

testFromNormalList = fromNormalList [1..5]
testFromNormalList2 = fromNormalList []

toNormalList EmptyList = []
toNormalList (ListElement a next) = a : (toNormalList next)

testToNormalList = toNormalList EmptyList
testToNormalList2 = toNormalList egzampleList