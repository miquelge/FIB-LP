
-- 1. -----------------------------------------------------------------

class (Eq p, Show p, Ord p) => Point p where
    sel :: Int -> p -> Double
    dim :: p -> Int
    child :: p -> p -> [Int] -> Int
    dist :: p -> p -> Double
    list2Point :: [Double] -> p

data Point3D = Point3D Double Double Double deriving (Eq, Show, Ord)


-- 2. -----------------------------------------------------------------

instance Point Point3D where
    sel 1 (Point3D x _ _) = x
    sel 2 (Point3D _ y _) = y
    sel 3 (Point3D _ _ z) = z
    dim (Point3D _ _ _) = 3
    child e1 e2 l = aux_child e1 e2 l 0
    dist (Point3D x1 y1 z1) (Point3D x2 y2 z2) = 
        sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
    list2Point (x:y:z:[]) = Point3D x y z

aux_child :: Point p => p -> p -> [Int] -> Int -> Int 
aux_child _ _ [] n = n
aux_child e1 e2 (x:xs) n =
    if (sel x e1 <= sel x e2) then aux_child e1 e2 xs (n*2 + 0)
    else aux_child e1 e2 xs (n*2 + 1)


-- 3. -----------------------------------------------------------------

data Kd2nTree a = Node a [Int] [Kd2nTree a] | Empty

instance (Eq a, Point a) => Eq (Kd2nTree a) where
    (==) = equal
equal :: Eq a => Kd2nTree a -> Kd2nTree a -> Bool 
equal Empty Empty = True
equal (Node a1 b1 f1) Empty = False
equal Empty (Node a2 b2 f2) = False
equal (Node a1 b1 f1) (Node a2 b2 f2) =
    if (a1 == a2) && (b1 == b2) then equalSons f1 f2
    else False
equalSons :: Eq a => [Kd2nTree a] -> [Kd2nTree a] -> Bool
equalSons [] [] = True
equalSons (x:xs) [] = False
equalSons [] (y:ys) = False
equalSons (x:xs) (y:ys) =
    if (length (x:xs) /= length (y:ys)) then False
    else (equal x y) && equalSons xs ys

instance (Show a, Point a) => Show (Kd2nTree a) where
    show = printTree
printTree :: Point a => Kd2nTree a -> String
printTree a = aux_printTree a (-1) ++ "\n"
aux_printTree :: Point a => Kd2nTree a -> Int -> String
aux_printTree Empty _ = ""
aux_printTree (Node a b fills) tabs =
    (printPoint a 1) ++ " " ++ show b
    ++ printTree_Sons fills (tabs+1) 0
printTree_Sons :: Point a => [Kd2nTree a] -> Int -> Int -> String
printTree_Sons [] _ _ = ""
printTree_Sons (x:xs) tabs son = 
    if (x /= Empty) then
        "\n" ++ indentation tabs ++ "<" ++ show son ++ ">"
        ++ aux_printTree x tabs
        ++ printTree_Sons xs tabs (son+1)
    else printTree_Sons xs tabs (son+1)
printPoint :: (Point a) => a -> Int-> String
printPoint p n 
    |(n == 1)     = "(" ++ show (sel n p) ++ printPoint p (n+1)
    |(n < dim p)  = "," ++ show (sel n p) ++ printPoint p (n+1)
    |(n == dim p) = "," ++ show (sel n p) ++ ")"
indentation :: Int -> String
indentation 0 = ""
indentation n = "\t" ++ indentation (n-1)


-- 4. -----------------------------------------------------------------

insert :: (Point p, Eq p) => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty x l = Node x l (empties (2^(length l)))
insert (Node p l sons) x l1 =
    if (place == Empty) then (Node p l
        ((take pos sons) ++ [Node x l1 emptySons] ++ (drop (pos+1) sons)))
    else (Node p l
        ((take pos sons) ++ [insert (sons!!pos) x l1] ++ (drop (pos+1) sons)))
    where
        pos = child p x l
        place = sons!!pos
        emptySons = empties (2^(length l))
empties :: Point p => Int -> [Kd2nTree p]
empties 0 = []
empties n = Empty:empties (n-1)

build :: Point p =>  [(p, [Int])]  -> Kd2nTree p
build l = auxBuild l Empty 
auxBuild :: Point p =>  [(p, [Int])]  -> Kd2nTree p -> Kd2nTree p
auxBuild [] tree = tree
auxBuild (x:xs) tree = auxBuild xs (insert tree (fst x) (snd x))

buildIni :: Point p => [([Double],[Int])] -> Kd2nTree p
buildIni l = build (map (\i -> (list2Point (fst i), (snd i))) l)


-- 5. -----------------------------------------------------------------

get_all :: Point p => Kd2nTree p -> [(p, [Int])]
get_all Empty = []
get_all (Node p x sons) = [(p, x)] ++ (concatMap get_all sons)


-- 6. -----------------------------------------------------------------

remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove Empty _ = Empty
remove (Node p l sons) q =
    if (p == q) then build (drop 1 (get_all (Node p l sons)))
    else (Node p l ((take pos sons) ++ [remove (sons!!pos) q] ++ (drop (pos+1) sons)))
    where
        pos = child q p l


-- 7. -----------------------------------------------------------------

contains :: (Eq p, Point p) => Kd2nTree p -> p -> Bool
contains Empty _ = False
contains (Node p l sons) q =
    if (p == q) then True
    else (any (\x -> contains x q) sons)
    where
        pos = child q p l


-- 8. -----------------------------------------------------------------

nearest :: (Eq p, Point p) => Kd2nTree p -> p -> p
nearest (Node p l sons) q
    | (p == q) = q
    | otherwise = auxNearest (get_all (Node p l sons)) p q

auxNearest :: (Point p, Eq p) => [(p,[Int])] -> p -> p -> p
auxNearest [] best p = best
auxNearest (x:xs) best p =
    if (p == (fst x)) then (fst x)
    else if ((dist p (fst x)) < (dist best (fst x)))
        then auxNearest xs (fst x) p
    else auxNearest xs (fst x) p


-- 9. -----------------------------------------------------------------

allinInterval :: (Eq p, Point p) => Kd2nTree p -> p -> [p]
allinInterval tree p1 p2 =
    filter (\i -> ((geq p1 i) && (get i p2))) (get_all_points tree)





get_all_points :: Point p => Kd2nTree p -> [p]
get_all Empty = []
get_all (Node p x sons) = [p] ++ (concatMap get_all_points sons)


geq :: Point p => p -> p -> Bool
geq p q = auxGeq p q 0

auxGeq :: Point p => p -> p -> Int -> Bool
auxGeq 



lessThan :: Point p => p -> p -> Bool
lessThan p q = 
    if (dim p \= dim q) then False
    else auxLessThan p q 1


auxLessThan :: Point p => p -> p -> Int -> Bool
auxLessThan p q n =
    
    if ((sel n p) < (sel n q)) then  True
    else if ((sel n p) > (sel n q)) then False
    else auxLessThan p q (i+1)




sort :: Point p => [p] -> [p]
sort [] = []
sort (x:xs) = (sort (filter (lessThan x) xs))
    ++ [x] ++ (sort (filter (greaterEqual x) xs))


exampleSet :: Kd2nTree Point3D
exampleSet = (Node (Point3D 3.0 (-1.0) 2.1) [1,3] [
                (Node (Point3D 3.0 5.1 0.0) [2] [
                    (Node (Point3D 1.8 1.1 (-2.0)) [1,2]
                        [Empty,Empty,Empty,Empty]),
                    (Node (Point3D 1.5 8.0 1.5) [1]
                        [Empty,Empty])]),
                (Node (Point3D 3.0 (-1.7) 3.1) [1,2,3]
                    [Empty, Empty,Empty,Empty,Empty,Empty,Empty,Empty]),
                (Node (Point3D 3.5 0.0 2.1) [3]
                    [Empty,Empty]),
                (Node (Point3D 3.5 2.8 3.1) [1,2] [
                        (Node (Point3D 3.3 2.8 2.5) [3]
                            [Empty,Empty]),
                        (Node (Point3D 3.1 3.8 4.8) [1,3]
                            [Empty,Empty,Empty,Empty]),
                        Empty,
                        (Node (Point3D 4.0 5.1 3.8) [2]
                            [Empty,Empty])])])


