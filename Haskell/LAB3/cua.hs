-- Cua

data Queue a = Queue [a] [a]
     deriving (Show)
 

create :: Queue a
create = Queue [] []


push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = Queue l1 (x : l2)


pop :: Queue a -> Queue a
pop (Queue (x:xs) l) = (Queue xs l)
pop (Queue [] l) = pop (Queue (reverse l) [])


top :: Queue a -> a
top (Queue (x:xs) _) = x
top (Queue [] l) = last l


empty :: Queue a -> Bool
empty (Queue l1 l2) = (length l1 == 0) && (length l2 == 0)


instance Eq a => Eq (Queue a)
    where (Queue a1 a2) == (Queue b1 b2) = (a1 ++ reverse a2) == (b1 ++ reverse b2)
