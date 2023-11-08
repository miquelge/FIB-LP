-- Funcions amb llistes


myLength :: [Int] -> Int
myLength [] = 0
myLength (_ : tail) = (1 + (myLength tail))


myMaximum :: [Int] -> Int
myMaximum [a] = a
myMaximum (a : tail) = 
    if (a >= myMaximum tail) then a
    else myMaximum tail


average :: [Int] -> Float
average [] = 0
average l = (fromIntegral (sum l)) / (fromIntegral (myLength l))


buildPalindrome :: [Int] -> [Int]
buildPalindrome l = (auxBuildInverse l) ++ l
auxBuildInverse :: [Int] -> [Int]
auxBuildInverse [] = []
auxBuildInverse (a : tail) = (auxBuildInverse tail) ++ [a]


remove :: [Int] -> [Int] -> [Int] 
remove [] _ = []
remove (a : xs) l =
    if elem a l then remove xs l
    else [a] ++ remove xs l


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (l : tail) = l ++ flatten tail


oddsNevens :: [Int] -> ([Int],[Int]) 
oddsNevens [] = ([],[])
oddsNevens (x : tail) = 
    if (x `mod` 2) == 0 then (y, [x] ++ z)
    else ([x] ++ y, z)
    where {(y, z) = oddsNevens tail}
--oddsNevens :: [Int] -> ([Int],[Int]) 
--oddsNevens [] = ([],[])
--oddsNevens (x : tail) = 
--    if (x `mod` 2) == 1 then ([x] ++ fst (oddsNevens tail), snd (oddsNevens tail))
--    else (fst (oddsNevens tail), [x] ++ snd (oddsNevens tail))


primeDivisors :: Int -> [Int]
primeDivisors x = auxPrimeDivisors x 2
auxPrimeDivisors :: Int -> Int -> [Int]
auxPrimeDivisors 1 _ = []
auxPrimeDivisors x d =
    if ((x `mod` d) == 0) then [d] ++ auxPrimeDivisors (divideix x d) (d + 1)
    else auxPrimeDivisors x (d + 1)
divideix :: Int -> Int -> Int
divideix x n =
    if (x `mod` n) == 0 then (divideix (x `div` n) n)
    else x

