import Data.Maybe
import Data.Either

data Point a = Double a
     deriving (Show)

     import Data.Maybe
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)