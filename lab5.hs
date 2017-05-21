nic :: a -> a
nic a = a

sklej f1 f2 = f2 f1

mySum a b = a+b
--nic a `sklej` f = f a

--test 1 = (nic 3 `sklej` (mySum 4)) == ((mySum 4) 3)
--test 2 = 3 `sklej` nic == 3
--test _ = False

--f :: a -> b
f a b = a:b

--f':: IO () => a -> b
f' a= ("extra info", a)
