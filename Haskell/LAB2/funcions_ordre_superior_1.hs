-- Funcions Ordre Superior 1


eql :: [Int] -> [Int] -> Bool
eql l1 l2 =
    if (length l1 == length l2) then and(zipWith (==) l1 l2)
    else False 


prod :: [Int] -> Int
prod l = foldl (*) 1 l


prodOfEvens :: [Int] -> Int
prodOfEvens l = foldl operacio 1 l
operacio :: Int -> Int -> Int
operacio x y =
    if (y `mod` 2) == 0 then (x * y)
    else x


powersOf2 :: [Int]
powersOf2 = iterate (*2) 1


scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldl (+) 0 (zipWith (*) l1 l2)


