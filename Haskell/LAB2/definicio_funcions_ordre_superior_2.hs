-- Definició Funcions Ordre Superior 2


countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = foldr (\x -> if (f x) then (+1) else (+0)) 0 l


pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l1 l2 = map (\f -> map f l1) l2


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l1 l2 = map (\x -> map (\f -> f x) l2) l1


filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f1 f2 n [] = n
filterFoldl f1 f2 n l =
    foldr (\x -> if f1 x then f2 x else id) n l


insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] e = [e]
insert f (x:xs) e =
    if (f e x) then e : x : xs
    else x : insert f xs e
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort _ [] = []
insertionSort f l = foldl (insert f) [] l
