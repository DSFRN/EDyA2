import Par
import Seq
import ListSeq

promedios :: Seq s => s Int -> s Float
promedios s = tabulateS (\i -> if i < len-1
                               then fromIntegral (nthS sums (i+1)) / fromIntegral (i+1)
                               else fromIntegral total / fromIntegral len) len
              where
                (sums, total) = scanS (+) 0 s
                len = lengthS s

-- promedios <2, 4, 3, 7, 9> = <2, 3, 3, 4, 5>
-- test = [2,4,3,7,9] :: [Int]


mayores :: Seq s => s Int -> Int
mayores s = reduceS (+) 0 (tabulateS f len :: [Int])
            where
              (x, y) = scanS max 0 s
              len    = lengthS s
              f i    = if i < len-1
                       then if (nthS s i) > (nthS x (i+1)) then 1 else 0
                       else if (nthS s i) > y then 1 else 0

-- mayores <1, 2, 5, 3, 5, 2, 7, 9> = 4
-- test = [1,2,5,3,5,2,7,9] :: [Int]


prodMat :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
prodMat (a1,a2,a3,a4) (b1,b2,b3,b4) =
  ((a1*b1)+(a2*b3), (a1*b2)+(a2*b4), (a3*b1)+(a4*b3), (a3*b2)+(a4*b4))

fibSeq :: Seq s => Int -> s Int
fibSeq n = mapS (\(a,b,c,d) -> a) (fst (scanS prodMat (1,1,1,0) (tabulateS (\_ -> (1,1,1,0)) n)))


reverseS :: Seq s => s a -> s a
reverseS s = tabulateS (\i -> nthS s (n - 1 - i)) n
             where n = lengthS s

aguaHist :: Seq s => s Int -> Int
aguaHist s = reduceS (+) 0 (tabulateS f (lengthS s) :: [Int])
             where
               ((maxL,_), (maxR',_)) = scanS max 0 s ||| scanS max 0 (reverseS s)
               maxR = reverseS maxR'
               f i  = max 0 (min (nthS maxL i) (nthS maxR i) - (nthS s i))

-- aguaHist <2,3,4,7,5,2,3,2,6,4,3,5,2,1> = 15
-- test = [2,3,4,7,5,2,3,2,6,4,3,5,2,1] :: [Int]


data Paren = Open | Close deriving Eq

-- Divide and Conquer
matchParen1 :: Seq s => s Paren -> Bool
matchParen1 s = (f s) == (0,0)
                where
                  f s = case showtS s of
                        EMPTY     -> (0,0)
                        ELT Open  -> (1,0)
                        ELT Close -> (0,1)
                        NODE l r  -> let (l1, l2) = f l
                                         (r1, r2) = f r
                                         (ml1, ml2) = max 0 (l1-r2) ||| max 0 (l2-r1)
                                         (mr1, mr2) = max 0 (r1-l2) ||| max 0 (r2-l1)
                                     in  ml1 + ml2 ||| mr1 + mr2


matchParen2 :: Seq s => s Paren -> Bool
matchParen2 s = (reduceS min 0 trail == 0) && (correct == 0)
                where 
                  s' = mapS (\i -> if (i == Open) then 1 else (-1)) s
                  (trail, correct) = scanS (+) 0 s'


sccml1 :: Seq s => s Int -> Int
sccml1 s = reduce max 0 subSeq
           where
             tupls = tabulateS (\i -> ((nthS s i, i),(nthS s i, i))) (lengthS s)
             maxS  = reduceS max (nthS s 0) s
             base  = ((max,0),(max,0))
             combine ((a1,a2),(b1,b2)) ((c1,c2),(d1,d2)) =
               if (b1 < c1) && (b2 == c2-1)
               then ((a1,a2),(d1,d2))
               else ((c1,c2),(d1,d2))
             (red, res) = scanS combine base tupls
             red_res = appendS red (singletonS res)
             subSeq = mapS (\(_,i),(_,j) -> j-i) red_res

-- Algoritmo de Kadane.
mcss :: (Num a, Ord a, Seq s) => s a -> a
mcss s = first (reduceS g (0,0,0,0) (mapS f s))
         where
           first (a,_,_,_) = a
           f x = (max x 0, max x 0, max x 0, x)
           g (a1,a2,a3,a4) (b1,b2,b3,b4) =
             (maximum [a1,b1, a3+b2], max a2 (a4+b2), max b3 (b4+a3), a4+b4)

mcssScan :: (Seq s) => s Int -> Int
mcssScan s = reduceS max 0 css
             where
               (prefSum, total) = scanS (+) 0 s
               allPrefSum     = appendS prefSum (singletonS total)
               (minPref, _)   = scanS min maxBound allPrefSum
               f i = if (i == 0) then 0 else (nthS allPrefSum i) - (nthS minPref i)
               css = tabulateS f (lengthS allPrefSum)


sccml2 :: Seq s => s Int -> Int
sccml2 s = mcss (tabulateS f len :: [Int])
           where
             len = lengthS s
             f 0 = 0
             f n | (nthS s n) > (nthS s (n-1)) = 1
                 | otherwise                   = negate len

{-
cantMultiplos :: Seq s => s Int -> Int
cantMultiplos s = reduceS (+) 0 (tabulateS forEachJ (lengthS s))
                  where
                    forEachJ j = reduceS (+) 0
                                 (tabulateS
                                   (\i -> if (mod (nthS s i) (nthS j i) == 0) then 1 else 0)
                                 j)
-}


maxE :: Seq s => (a -> a -> Ordering) -> s a -> a
maxE cmp s = reduceS mayor (nthS s 0) s
             where
               mayor x y = case cmp x y of
                             GT -> x
                             _  -> y

maxS :: Seq s => (a -> a -> Ordering) -> s a -> Int
maxS cmp s = reduceS mayor (nthS s 0, 0) (tabulateS (\i -> (nthS s i, i)) (lengthS s))
             where
               mayor (x,i) (y,j) = case cmp x y of
                                     GT -> (x,i)
                                     _  -> (y,j)

group :: Seq s => (a -> a -> Ordering) -> s a -> s a
group cmp s = mapS (nthS s) (filterS f (tabulateS id (lengthS s)))
              where
                f i = (i == 0) || (cmp (nthS s i) (nthS s (i-1)) /= EQ)

