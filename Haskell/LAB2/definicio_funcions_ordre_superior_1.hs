-- Definició Funcions Ordre Superior 1


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f n [] = n
myFoldl f n (x:tail) = myFoldl f (n `f` x) tail


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f n [] = n
myFoldr f n (x:tail) = x `f` (myFoldr (f) n tail)


myIterate :: (a -> a) -> a -> [a]
myIterate f n = [n] ++ myIterate f (f n)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f g x =
    if (f x) then x
    else myUntil f g (g x)


myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f l = foldr (\x xs -> (f x):xs) [] l


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f l = foldr (\x xs -> if (f x) then x:xs else xs) [] l


myAll :: (a -> Bool) -> [a] -> Bool
myAll f l = and (myMap f l)


myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = or (myMap f l)


myZip :: [a] -> [b] -> [(a, b)]
myZip xs ys = foldl (\r x a -> r (f x a)) id xs (const []) ys
    where
        f x r []     = []
        f x r (y:ys) = (x,y) : r ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f l1 l2 = myMap (\(x, y) -> f x y) (myZip l1 l2)


