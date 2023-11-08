-- Ús de llistes per comprensió


myMap :: (a -> b) -> [a] -> [b]
myMap f l = [x | y <- l, let x = f y]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [a | a <- l, f a]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f l1 l2 = [f x y | (x, y) <- zip l1 l2]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l1 l2 = [(x,y) | x <- l1, y <- l2, mod x y == 0]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]