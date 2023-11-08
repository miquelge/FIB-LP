-- Llistes Infinites


ones :: [Integer]
ones = iterate id 1


nats :: [Integer]
nats = iterate (+1) 0


ints :: [Integer]
ints = 0 : concat [[x, (-x)] | x <- (iterate (+1) 1)]


triangulars :: [Integer]
triangulars = [0] ++ map (\x -> ((x * (x + 1)) `div` 2)) (iterate (+1) 1)


factorials :: [Integer]
factorials = 1 : scanl1 (*) (iterate (+1) 1)


fibs :: [Integer]
fibs = scanl (+) 0 (1:fibs)


primes :: [Integer]
primes = filter isPrime nats
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = let maxDiv = floor (sqrt (fromIntegral n))
    in all (\x -> (n `rem` x) /= 0) (takeWhile (<= maxDiv) primes)


hammings :: [Integer]
hammings = 1 : (merge (merge (map (*2) hammings) (map (*3) hammings)) (map (*5) hammings))
merge :: [Integer] -> [Integer] -> [Integer]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | (x < y) = x : (merge xs (y:ys))
    | (x > y) = y : (merge (x:xs) ys)
    | otherwise = merge (x:xs) ys


lookNsay :: [Integer]
lookNsay = iterate (mira_digues) 1
mira_digues :: Integer -> Integer
mira_digues x = listToInt (comptar (intToList x) 0 0)
comptar :: [Integer] -> Integer -> Integer -> [Integer]
comptar [] last ap = [ap, last]
comptar (x:xs) last ap = 
    if (last == 0) then comptar xs x 1
    else if (x == last) then comptar xs last (ap+1)
    else ap : last : comptar xs x 1
intToList :: Integer -> [Integer]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]
listToInt :: [Integer] -> Integer
listToInt = foldl addDigit 0
   where addDigit num d = 10*num + d


tartaglia :: [[Integer]]
tartaglia = iterate (seguent) [1]
seguent :: [Integer] -> [Integer]
seguent l = 1 : auxTartaglia l
auxTartaglia :: [Integer] -> [Integer]
auxTartaglia (x1 : x2 : xs) = (x1+x2) : auxTartaglia (x2:xs)
auxTartaglia _ = [1]



