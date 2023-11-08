-- Arbre Binari

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ x y) = 1 + size x + size y


height :: Tree a -> Int
height Empty = 0
height (Node _ x y) = 1 + max (height x) (height y)


equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node a _ _) = False
equal (Node a _ _) Empty = False
equal (Node a fa1 fa2) (Node b fb1 fb2) =
    if not (a == b) then False
    else (equal fa1 fb1) && (equal fa2 fb2)


isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic a b = 
    if (equal a b) then True
    else (equal a (change_sons b))
change_sons :: Tree a -> Tree a
change_sons Empty = Empty
change_sons (Node a f1 f2) = (Node a f2 f1)


preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a f1 f2) = [a] ++ preOrder f1 ++ preOrder f2


inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a f1 f2) = inOrder f1 ++ [a] ++ inOrder f2


postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a f1 f2) = postOrder f1 ++ postOrder f2 ++ [a]


breadthFirst :: Tree a -> [a]
breadthFirst a = auxBreadthFirst [a]
auxBreadthFirst :: [Tree a] -> [a]
auxBreadthFirst [] = []
auxBreadthFirst ((Empty):xs) = auxBreadthFirst xs
auxBreadthFirst ((Node a f1 f2):xs) = [a] ++ (auxBreadthFirst (xs ++ [f1, f2]))


build :: Eq a => [a] -> [a] -> Tree a
build _ [] = Empty
build (x:pre) ino = (Node x (build leftPreordre leftInordre) (build rightPreordre rightInordre))
    where
        leftInordre = takeWhile (/=x) ino
        leftPreordre = take (length leftInordre) pre
        rightPreordre = drop (length leftInordre) pre
        rightInordre = tail (dropWhile (/=x) ino)


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap f Empty Empty = Empty
overlap f (Node a f1 f2) Empty = (Node a (overlap f f1 Empty) (overlap f f2 Empty))
overlap f Empty (Node a f1 f2) = (Node a (overlap f f1 Empty) (overlap f f2 Empty))
overlap f (Node a fa1 fa2) (Node b fb1 fb2) = (Node (a `f` b) (overlap f fa1 fb1) (overlap f fa2 fb2))

