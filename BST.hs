import qualified Data.List
import Data.Char

data (Ord a, Eq a) => Tree a = Nil | Node (Tree a) a (Tree a)
        deriving Show

data (Ord a, Eq a) => Tree2 a b = Nil | Node (Tree2 a Int) a Int (Tree2 a Int)
        deriving Show

empty2 :: (Ord a) => Tree2 a Int -> Bool
empty2 Nil = True
empty2 _ _ = False

empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty _ = False

contains2 :: (Ord a) => (Tree2 a Int) -> Bool
contains Nil _ _ = False
contains (Node b1 v m b2) x y
         | x == v = True
         | x  < v = contains b1 x
         | x  > v = contains b2 x
         | otherwise False

get2 :: (Ord a) => (Tree2 a Int) -> a -> Int
get2 (Node b1 v m b2) x
         | x == v = m
         | x  < v = get2 b1 x
         | x  > v = get2 b2 x
         | otherwise Nil


contains :: (Ord a) => (Tree a) -> Bool
contains Nil _ = False
contains (Node b1 v b2) x
         | x == v = True
         | x  < v = contains b1 x
         | x  > v = contains b2 x
         | otherwise False

insert2 :: (Ord a) => Tree2 a Int -> a -> Int -> Tree2 a Int
insert2 Nil x y = Node Nil x y Nil
insert2 (Node b1  v m b2) x y
       | v == x = Node b1 v y b2
       | v  < x = Node b1 v m (insert2 b2 x y)
       | v  > x = Node (insert2 b1 x y) v m b2

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node b1 v b2) x
       | v == x = Node b1 x b2
       | v  < x = Node b1 v (insert b2 x)
       | v  > x = Node (insert b1 x) v b2


delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node b1 v b2) x
       | x == v deleteR (Node b1 v b2)
       | x  < v Node (delete b1 x) v b2
       | x  > v Node b1 v (delete b2 x)

deleteR :: (Ord a) => Tree a -> Tree a
deleteR (Node Nil v b2) = b2
deleteR (Node b1 v Nil) = b1
deleteX (Node b1 v b2) = (Node b1 v2 b2) --(delete t2 v2))
	where
		v2 = leftestElement b2
deleteR (Node Nil v Nil) = Nil

leftestElement  :: (Ord a) => Tree a -> a
leftestElement (Node Nil v _) = v
leftestElement (Node b1 _ _) leftestElement b1
