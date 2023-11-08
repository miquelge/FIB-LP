-- Ordenacio


insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (a : tail) x = 
    if (x <= a) then x : a : tail
    else a : insert tail x

isort :: [Int] -> [Int]
isort l = auxIsort l []
auxIsort :: [Int] -> [Int] -> [Int]
auxIsort [] l = l
auxIsort (x : tail) l = auxIsort tail (insert l x)



remove :: [Int] -> Int -> [Int]
remove (x : tail) y =
    if (x == y) then tail
    else x : remove tail y

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = min : (ssort l1)
    where   min = minimum l
            l1 = remove l min



merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (x : l1) (y : l2) =
    if (x <= y) then x : merge l1 (y : l2)
    else y : merge (x : l1) l2

msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort l = merge (msort l1) (msort l2)
    where   pair = splitAt (length l `div` 2) l
            l1 = (fst pair)
            l2 = (snd pair)



qsort :: [Int] -> [Int]
qsort [] = []
qsort [a] = [a]
qsort (p : tail) = l1 ++ [p] ++ l2
    where   l1 = qsort (filter (< p) tail)
            l2 = qsort (filter (>= p) tail)

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort [a] = [a]
genQsort (p : tail) = l1 ++ [p] ++ l2
    where   l1 = genQsort (filter (< p) tail)
            l2 = genQsort (filter (>= p) tail)


