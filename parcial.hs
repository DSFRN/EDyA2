import Par

exclamationsOks :: Seq Char -> Bool
exclamationsOks s = (reduce (&&) True (map (>= 0) sums)) && (res == 0)
                    where
                    f x | (x == '¡') = 1
                        | (x == '!') = (-1)
                        | otherwise  = 0
                    s' = map f s
                    (sums, res) = scan (+) 0 s'


highestIndexes :: Seq Float -> Seq Nat
highestIndexes s = filter (>= 0) indexes
                   where
                      (sums,_) = scan (+) 0 s
                      n = length s
                      f 0 = if (nth s 0) > 0                             then 0 else -1
                      f i = if (nth s i) > (nth sums i)/(fromIntegral i) then i else -1
                      indexes = tabulate f n

tam t E          = 0
tam t (L a)      = 1
tam t (N i _ _ ) = i

merge t  E  = t
merge E  t  = t
merge t1 t2 = (N (tam t1 + tam t2) t1 t2)

split P T@(N n _ _)  = split' P T n

split' P E i         = E
split' P (L a) i     = if (P a) then (L (i-1,a), E) else (E, L (i-1,a))
split' P (N n l r) i = let ((l1,l2),(r1,r2)) = split' P l (i - tam r) ||| split' P r i
                       in  (merge (l1,r1), merge(l2,r2))


entradas s x =
  let
    (sums,_) = scan (+) 0 s
    validDesc i = (i > 0) && (nth sums i) > (((nth s (i-1)) * i)
    desc = tabulate (\i -> if (validDesc i) then (nth s i) else 0) (length s)
    allDesc = reduce (+) 0 desc
    calcPrice = fromIntegral (nthS s i) * (if (validDesc i) then (x/2) else x)
    ganancia = tabulate calcPrice (length s)
    allGan = reduce (+) 0 ganancia
  in
    (allGan,allDesc)


isValue k' v' E = False
isValue k' v' (N l (k,t) r) =
    if (k' == k)
    then buscarT v' t
    else if (k' > k) then isValue r else isValue l
  where
    buscarT v' t =
      case t of
         E -> False
         Leaf v -> if (v' == v) then True else False
         (Node _ lt rt) -> let (lt',rt') = buscarT v' lt ||| buscarT v' rt
                           in (lt' && rt')
                 

toMap :: Ord k => MultiDic k v -> Tree (k, Int, v)
toMap E = Empty
toMap (N left k vals right) =
    append (append (toMap left) (indexVals k vals)) (toMap right)


indexVals :: k -> Tree v -> Tree (k, Int, v)
indexVals k t = go 0 t
  where
    go _ Empty        = Empty
    go i (Leaf v)     = Leaf (k, i-1, v) 
    go i (Node n l r) = Node n (go (i - tam r) l) (go i r)


mapReduceIndex f op t@(Node n _ _) = mapReduceIndex' f op n t

mapReduceIndex' f op i (Leaf a)     = f (i-1) a
mapReduceIndex' f op i (Node n l r) = 
    let (l', r') = mapReduceIndex' f op (i - tam r) l ||| mapReduceIndex' f op i r 
    in  op l' r'

