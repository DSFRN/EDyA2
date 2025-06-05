module Prac6 where

import Par

size :: BTree a -> Int
size Empty          = 0
size (Node h _ _ _) = h

-- (1) Secuencias
data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show
btree = Node 5 (Node 3 (Node 1 Empty 91 Empty) 92 (Node 1 Empty 93 Empty)) 94 (Node 1 Empty 95 Empty)

nth :: BTree a -> Int -> a
nth (Node _ l a r) n | n == (size l)+1  = a
                     | n <  (size l)+1  = nth l n
                     | otherwise        = nth r (n - ((size l)+1))

cons :: a -> BTree a -> BTree a
cons a Empty          = Node 1 Empty a Empty
cons a (Node h l b r) = Node (h+1) (cons a l) b r

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Node 1 Empty (f 0) Empty
tabulate f n = cons (f n) (tabulate f (n-1))

mapT :: (a -> b) -> BTree a -> BTree b
mapT _ Empty          = Empty
mapT f (Node h l a r) = let ((l', a'), r') = (mapT f l) ||| (f a) ||| (mapT f r)
                         in Node h l' a' r'

takeT :: Int -> BTree a -> BTree a
takeT _ Empty          = Empty
takeT n (Node h l a r) | n == (size l)+1  = Node h l a Empty
                       | n >  (size l)+1  = Node h l a (takeT (n - ((size l)+1)) r)
                       | otherwise        = takeT n l

dropT :: Int -> BTree a -> BTree a
dropT _ Empty = Empty
dropT n (Node h l a r) | n == (size l)  = Node (h-n) Empty a r
                       | n <  (size l)  = Node (h-n) (dropT n l) a r
                       | otherwise      = dropT (n - ((size l)+1)) r

-- (2) Divide and Conquer
data Tree a = E | Leaf a | Join (Tree a) (Tree a)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce _ _ e E          = e
mapreduce f _ _ (Leaf a)   = f a
mapreduce f g e (Join l r) = let (l', r') = (mapreduce f g e l) ||| (mapreduce f g e r)
                              in g l' r'

trimax :: (Num a, Ord a) => a -> a -> a -> a
trimax a b c = max (max a b) c

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = let fst' (a,_,_,_) = a
             f x = (max x 0, max x 0, max x 0, x)
             g (a,b,c,d) (w,x,y,z) =
               (trimax (c+x) a w, max b (d+x), max y (c+z), d+z)
          in fst' (mapreduce f g (0,0,0,0) t)

-- (3) Ayudamos a YPF


