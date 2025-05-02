module Prac4 where

import Data.Bool
import Data.List

data BTree a = EB | LeafB a | NodeB (BTree a) a (BTree a)
data BST a   = E  | Leaf a  | Node (BST a) a (BST a)

-- (1)
completo :: a -> Int -> BTree a
completo _ 0 = EB
completo a 1 = LeafB a
completo a n = let ch = completo a (n-1)
               in (NodeB ch a ch)

-- (2)
maximumBST :: (Ord a) => BST a -> a
maximumBST E            = undefined
maximumBST (Leaf a)     = a
maximumBST (Node l a E) = a
maximumBST (Node l _ r) = maximumBST r

--
inorder :: BST a -> [a]
inorder E            = []
inorder (Leaf a)     = [a]
inorder (Node l a r) = (inorder l) ++ [a] ++ (inorder r)

checkBST :: (Ord a) => BST a -> Bool
checkBST E = True
checkBST t = let elementos = (inorder t)
             in (elementos == (sort elementos))

--
splitBST :: (Ord a) => BST a -> a -> (BST a, BST a)
splitBST E _ = (E, E)
splitBST (Node l y r) x | (x == y) = (Node l y r, r)
                        | (x < y)  = let (l', r') = splitBST l x
                                     in (l', Node r' y r)
                        | (x > y)  = let (l', r') = splitBST r x
                                     in (Node l y l', r')

--
value :: BST a -> a
value E            = undefined
value (Leaf a)     = a
value (Node _ a _) = a

join :: (Ord a) => BST a -> BST a -> BST a
join E E   = E
join E t   = t
join t E   = t
join t1 t2 | a <= b    = Node t1 a t2
           | otherwise = Node t2 b t1
           where
                a = value t1
                b = value t2

-- (3)
member :: (Ord a) => a -> BST a -> Bool
member _ E = False
member b (Leaf a) = if (a == b)
                    then True
                    else False
member b (Node l a r) = if (a > b)
                        then member b l
                        else member b r

-- (4)


-- (5)
data Color = R | B
data RBT a = ERB | RBNode Color (RBT a) a (RBT a)

data T123 a =
       E123
     | Node2 a (T123 a) (T123 a)
     | Node3 a a (T123 a) (T123 a) (T123 a)
     | Node4 a a a (T123 a) (T123 a) (T123 a) (T123 a)

fromRBT :: RBT a -> T123 a
fromRBT ERB = E123
fromRBT (RBNode _ (RBNode _ l1 b r1) a (RBNode _ l2 c r2)) =
        Node4 a b c (fromRBT l1) (fromRBT r1) (fromRBT l2) (fromRBT r2)
fromRBT (RBNode _ (RBNode _ l1 b r1) a r) =
        Node3 a b (fromRBT l1) (fromRBT r1) (fromRBT r)
fromRBT (RBNode _ l a (RBNode _ l2 c r2)) =
        Node3 a c (fromRBT l) (fromRBT l2) (fromRBT r2)
fromRBT (RBNode _ l a r) =
        Node2 a (fromRBT l) (fromRBT r)

-- (6)
data Heap a = EH | HN Int a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Int
rank EH           = 0
rank (HN r _ _ _) = r

makeH x a b = if (rank a) > (rank b)
              then HN ((rank b) + 1) x a b
              else HN ((rank a) + 1) x b a

mergeH :: (Ord a) => Heap a -> Heap a -> Heap a
mergeH EH EH = EH
mergeH h1 EH = h1
mergeH EH h2 = h2
mergeH h1@(HN r1 x a1 b1) h2@(HN r2 y a2 b2) = if x <= y
                                               then makeH x a1 (mergeH b1 h2)
                                               else makeH y a2 (mergeH h1 b2)

inorderHeap :: Heap a -> [a]
inorderHeap EH             = []
inorderHeap (HN _ a h1 h2) = inorderHeap h1 ++ [a] ++ inorderHeap h2

fromList :: (Ord a) => [a] -> Heap a
fromList []     = EH
fromList (x:xs) = mergeH (HN 0 x EH EH) (fromList xs)

-- (7)
data PHeaps a = PE | Root a [PHeaps a]

isPHeap :: (Ord a) => PHeaps a -> Bool
isPHeap PE           = True
isPHeap (Root a chs) = (isRootLeqChild a chs) && (areChildrenPH chs)
  where
    isRootLeqChild a []                  = True
    isRootLeqChild a ((Root x chs'):chs) = (a <= x) && (isRootLeqChild a chs)

    areChildrenPH []       = True
    areChildrenPH (ch:chs) = (isPHeap ch) && (areChildrenPH chs)

--
mergePH :: (Ord a) => PHeaps a -> PHeaps a -> PHeaps a
mergePH PE PE  = PE
mergePH ph1 PE = ph1
mergePH PE ph2 = ph2
mergePH root1@(Root a chs1) root2@(Root b chs2) = if a <= b
                                                   then Root a ([root2]++chs1)
                                                   else Root b ([root1]++chs2)

--
insertPH :: (Ord a) => PHeaps a -> a -> PHeaps a
insertPH PE a = (Root a [])
insertPH ph a = mergePH (Root a []) ph

--
concatHeaps :: (Ord a) => [PHeaps a] -> PHeaps a
concatHeaps []     = PE
concatHeaps [x]    = x
concatHeaps (x:xs) = mergePH x (concatHeaps xs)

--
delMin :: (Ord a) => PHeaps a -> Maybe (a, PHeaps a)
delMin PE           = Nothing
delMin (Root a chs) = Just (a, concatHeaps chs)
