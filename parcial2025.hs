module Parcial2025 where

data Scapegoat a = E | N a Int (Scapegoat a) (Scapegoat a) deriving (Eq, Show)


size :: Scapegoat a -> Int
size E           = 0
size (N _ n _ _) = n


isBST :: (Ord a) => Scapegoat a -> Bool
isBST E                                   = True
isBST (N _ _ E E)                         = True
isBST (N a _ l@(N b _ _ _) E)             = (a >= b) && isBST l
isBST (N a _ E r@(N c _ _ _))             = (a < c)  && isBST r
isBST (N a _ l@(N b _ _ _) r@(N c _ _ _)) = (a >= b && a < c) && isBST l && isBST r


isScapegoatTree :: (Ord a) => Scapegoat a -> Bool
isScapegoatTree E                                     = True
isScapegoatTree (N _ _ E E)                           = True
isScapegoatTree (N a n l@(N b nl _ _) E)              = (a >= b) && (3*nl <= 2*n) && isScapegoatTree l
isScapegoatTree (N a n E r@(N c nr _ _))              = (a < c)  && (3*nr <= 2*n) && isScapegoatTree r
isScapegoatTree (N a n l@(N b nl _ _) r@(N c nr _ _)) = (a >= b && a < c) &&
                                                        (3*nl <= 2*n) &&
                                                        (3*nr <= 2*n) &&
                                                        isScapegoatTree l &&
                                                        isScapegoatTree r

test = N 10 10 (N 7 6 (N 5 4 (N 3 2 (N 1 1 E E) E) (N 6 1 E E)) (N 8 1 E E)) (N 15 3 (N 12 1 E E) (N 20 1 E E))


member :: (Ord a) => a -> Scapegoat a -> Bool
member _ E = False
member x (N a _ l r) | (x > a)   = False || member x r
                     | (x < a)   = False || member x l
                     | otherwise = True


rebuild :: Scapegoat a -> Scapegoat a
rebuild = rebuild' . flattenInOrder

rebuild' :: [a] -> Scapegoat a
rebuild' [] = E
rebuild' xs = (N (xs !! mid) long (rebuild' left) (rebuild' right))
              where
                long  = length xs
                mid   = div long 2
                left  = (take mid xs)
                right = (drop (mid+1) xs)

flattenInOrder :: Scapegoat a -> [a]
flattenInOrder E = []
flattenInOrder (N a _ l r) = flattenInOrder l ++ [a] ++ flattenInOrder r

test2 = N 6 4 (N 3 3 (N 1 1 E E) (N 5 1 E E)) E


insert :: (Ord a) => a -> Scapegoat a -> Scapegoat a
insert x E = (N x 1 E E)
insert x (N a n l r) | (x <= a) = if (3*(nl+1) > 2*(n+1))
                                  then rebuild (N a (n+1) (insert x l) r)
                                  else         (N a (n+1) (insert x l) r)
                     | (x > a)  = if (3*(nr+1) > 2*(n+1))
                                  then rebuild (N a (n+1) l (insert x r))
                                  else         (N a (n+1) l (insert x r))
                      where
                        nl = size l
                        nr = size r

