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
data Tree a = Empt | Leaf a | Join (Tree a) (Tree a)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce _ _ e Empt       = e
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

-- (3)
t1 = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)
t2 = Join (Join (Leaf 25) (Leaf 5)) (Leaf 30)
tt = Join (Leaf t1) (Leaf t2)

reduce :: (a -> a -> a) -> a -> Tree a -> a
reduce _ e Empt       = e
reduce _ _ (Leaf a)   = a
reduce f e (Join l r) = let (l', r') = (reduce f e l) ||| (reduce f e r)
                         in f l' r'

-- sufijos :: (Num a) => Tree a -> Tree (Tree a)

-- conSufijos :: (Num a) => Tree a -> Tree (a, Tree a)

maxT :: (Num a, Ord a) => Tree a -> a
maxT t = reduce max 0 t

maxAll :: (Num a, Ord a) => Tree (Tree a) -> a
maxAll t = mapreduce maxT max 0 t

-- (4)
data T a = E | N (T a) a (T a)
altura :: T a -> Int
altura E         = 0
altura (N l x r) = 1 + max(altura l, altura r)

-- a)
combinar :: T a -> T a -> T a
combinar E t2         = t2
combinar (N l a r) t2 = N (combinar l r) a t2

-- b)
filterT :: (a -> Bool) -> T a -> T a
filterT _ E         = E
filterT p (N l a r) = let (l', r') = (filterT p l) ||| (filterT p r)
                      in if p a
                         then N l' a r'
                         else combinar l' r'

-- c)
quicksortT :: (Num a, Ord a) => T a -> T a
quicksortT E         = E
quicksortT (N l a r) = let (l1, r1) = filterT (<= a) l ||| filterT (<= a) r
                           (l2, r2) = filterT (> a) l ||| filterT (> a) r
                           (men, may) =
                             quicksortT (combinar l1 r1) ||| quicksortT (combinar l2 r2)
                        in N men a may

-- (5)
-- splitAt :: BTree a -> Int -> (BTree a, BTree a)
-- rebalance :: BTree a → BTree a
