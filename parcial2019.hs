module Parcial2019 where

data Treap p k = E | N (Treap p k) p k (Treap  p k) deriving Show

key :: Treap p k -> k
key E           = undefined
key (N _ _ k _) = k

priority :: Treap p k -> p
priority E           = undefined
priority (N _ p _ _) = p

-- Tengo que expresar la recurrencia correspondiente al trabajo.
isTreap :: (Ord k, Ord p) => Treap p k -> Bool
isTreap E                                       = True
isTreap (N l@(N _ pl kl _) p k r@(N _ pr kr _)) = (p >= pl && p >= pr) &&
                                                  (k > kl && k < kr) &&
                                                  isTreap l && isTreap r

rotateL :: Treap k p -> Treap k p
rotateL E                         = E
rotateL (N l p k (N l' p' k' r')) = (N (N l p k l') p' k' r')

rotateR :: Treap k p -> Treap k p
rotateR E                         = E
rotateR (N (N l' p' k' r') p k r) = (N l' p' k' (N r' p k r))


insert :: (Ord k, Ord p) => k -> p -> Treap p k -> Treap p k
insert k  p  E           = (N E p k E)
insert k' p' (N l p k r) | k' > k    = fixRight (N l p k (insert k' p' r))
                         | k' < k    = fixLeft  (N (insert k' p' l) p k r)
                         | otherwise = (N l p k r)

fixRight :: (Ord p) => Treap p k -> Treap p k
fixRight E             = E
fixRight t@(N l p k r) = case r of
                         (N _ p' _ _) -> if p' >= p
                                         then rotateL t
                                         else t
                         E            -> t

fixLeft :: (Ord p) => Treap p k -> Treap p k
fixLeft E             = E
fixLeft t@(N l p k r) = case l of 
                        (N _ p' _ _) -> if p' >= p
                                        then rotateR t
                                        else t
                        E            -> t

split :: (Ord k, Ord p, Num p) => k -> Treap p k -> (Treap p k, Treap p k)
split x E             = (E, E)
split x t@(N _ p _ _) = (l', r')
                        where (N l' p' k' r') = insert x (p+1) t

test = insert 'e' 0 (insert 'a' 2 (insert 'c' 4 (insert 'j' 7 (insert 'h' 9 E))))
testSplit = split 'd' test
