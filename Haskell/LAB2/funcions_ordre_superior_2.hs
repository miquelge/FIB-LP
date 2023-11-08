-- Funcions Ordre Superior 2


flatten :: [[Int]] -> [Int] 
flatten l = foldl (++) [] l


myLength :: String -> Int
myLength l = foldl (\x y -> x + 1) 0 l


myReverse :: [Int] -> [Int]
myReverse l = foldl (\x y -> y : x) [] l
--myReverse l = foldl (flip (:)) [] l


countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (\lIn -> length (filter (==x) lIn)) l


firstWord :: String -> String
firstWord l = takeWhile (/= ' ') (dropWhile (== ' ') l)
--firstWord l = head (words l)




