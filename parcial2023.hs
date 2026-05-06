module Parcial2023 where

type Interval = (Int, Int)
data ITree = E | N ITree Interval ITree deriving Show

right :: ITree -> Int
right E             = undefined
right (N _ (_,b) r) = case r of
                      E -> b
                      _ -> right r

test = (N (N E (1,3) E) (5,7) (N E (10, 12) E))

-- Dar la recurrencia correspondiente al trabajo realizado.
checkIT :: ITree -> Bool
checkIT E                           = True
checkIT (N E (a,b) E)               = (a <= b)
checkIT (N E (a,b) r@(N _ (x,_) _)) = (a <= b) && (x > b+1) && checkIT r
checkIT (N l@(N _ (_,y) _) (a,b) E) = (a <= b) && (y < a-1) && checkIT l
checkIT (N l@(N _ (_,y) _) (a,b) r@(N _ (x,_) _)) = (a <= b) && (y < a-1) && (x > b+1) &&
                                                    checkIT l && checkIT r

splitMax :: ITree -> (Interval, ITree)
splitMax E         = undefined
splitMax (N l i E) = (i, l)
splitMax (N l i r) = (maxI, N l i r')
                     where (maxI, r') = splitMax r

merge :: ITree -> ITree -> ITree
merge E E = E
merge E r = r
merge l E = l
merge l r = (N l' maxI r)
            where (maxI, l') = splitMax l

delElem :: ITree -> Int -> ITree
delElem E _             = E
delElem (N l (a,b) r) x | (a == b) && (a == x) = merge l r
                        | (a == x)             = (N l (a+1,b) r)
                        | (b == x)             = (N l (a,b-1) r)
                        | (x < a)              = (N (delElem l x) (a,b) r)
                        | (x > b)              = (N l (a,b) (delElem r x))
                        | otherwise            = merge (N l (a,x-1) E) (N E (x+1,b) r)
