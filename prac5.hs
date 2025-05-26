module Prac5 where

-- (1)
  tad List (A:Set) where
    import Bool
   |nil     : List A
   |cons    : A -> List A -> List A
    null    : List A -> Bool
    head    : List A -> A
    rail    : List A -> List A
    inList  : List A -> A -> Bool
    delList : List A -> A -> List A

-- especificación algebráica:
  null nil             = True
  null (cons x l)      = False
  head (cons x l)      = x
  tail (cons x l)      = l
  inList nil x         = False
  inList (cons y l) x  = if y == x
                         then True
                         else inList l x
  delList (cons y l) x = if y == x
                         then delList l x
                         else cons y (delList l x)

-- especificación como modelo: (secuencias)
  nil                                = ()
  cons    x (x1,...,xn)              = (x,x1,...,xn)
  null    (x1,...,xn)                = True  (Caso n = 0)
                                     = Falso (Caso n > 0)
  head    (x1,...,xn)                = x1
  tail    (x1,x2,...,xn)             = (x2,...,xn)
  inList  (x1,...,xn) x              = True  (Caso x ∈ L)
                                     = False (Caso x ∉ L)
  delList (x1,...,x,...,x,...,xn) x  = (x1,...,xn)


-- (2)
  tad Stack (A:Set) where
    import Bool
   |empty   : Stack A
   |push    : A -> Stack A -> Stack A
    isEmpty : Stack A -> Bool
    top     : Stack A -> A
    pop     : Stack A -> Stack A

-- especificación algebráica:
  isEmpty empty      = True
  isEmpty (push x s) = False
  top (push x s)     = x
  pop (push x s)     = s

-- especificación como modelo: (secuencias)
  empty                   = ()
  push    x (x1,...,xn)   = (x,x1,...,xn)
  isEmpty (x1,...,xn)     = True  (Caso n = 0)
                          = False (Caso n > 0)
  top     (x1,...,xn)     = x1
  pop     (x1,x2,...,xn)  = (x2,...,xn)


-- (3)
  tad Conjunto (A:Set) where
    import Bool
   |vacío       : Conjunto A
   |insertar    : A -> Conjunto A -> Conjunto A
    borrar      : A -> Conjunto A -> Conjunto A
    esVacío     : Conjunto A -> Bool
    unión       : Conjunto A -> Conjunto A -> Conjunto A
    interseción : Conjunto A -> Conjunto A -> Conjunto A
    resta       : Conjunto A -> Conjunto A -> Conjunto A

-- especificación algebráica
  insertar x (insertar x c)       = insertar x c
  insertar x (insertar y c)       = insertar y (insertar x c)
  borrar x vacío                  = vacío
  borrar x (insertar y c)         = if y == x
                                    then borrar x c
                                    else insertar y (borrar x c)
  esVacío vacío                   = True
  esVacío (insertar x c)          = False
  unión vacío vacío               = vacío
  unión c c'                      = unión c' c
  unión c vacío                   = c
  unión c (insertar x c')         = unión (insertar x c) c'
  pertenece vacío                 = False
  pertenece x (insertar y c)      = if x == y
                                    then True
                                    else pertenece x c
  intersección c c'               = intersección c' c
  intersección c vacío            = vacío
  intersección c (insertar x c')  = if x pertenece c
                                    then insertar x (intersección c c')
                                    else intersección c c'
  resta vacío c                   = vacío
  resta c vacío                   = c
  resta c (insertar x c')         = resta (borrar x c) c'


-- (4)
  tad PQ (V:Set, K:Ordered Set) where
    import Bool
   |vacía   : PQ V K
   |poner   : V -> K -> PQ V K -> PQ V K
    primero : PQ V K -> V
    sacar   : PQ V K -> PQ V K
    esVacía : PQ V K -> Bool
    unión   : PQ V K -> PQ V K -> PQ V K

-- especificaciones algebráicas:
  poner v k (poner v' k' q)            = if k == k'
                                         then poner v' k' q
                                         else poner v' k' (poner v k q)
  primero (poner v k vacía)            = v
  primero (poner v k (poner v' k' q))  = if k > k'
                                         then primero (poner v k q)
                                         else primero (poner v' k' q)
  sacar (poner v k vacía)              = vacía
  sacar (poner v k (poner v' k' q))    = if k > k'
                                         then poner v' k' (sacar (poner v k q))
                                         else if k' > k
                                              then poner v k (sacar (poner v' k' q))
                                              else sacar (poner v' k' q)
  esVacía vacía                        = True
  esVacía (poner v k q)                = False
  unión vacía vacía                    = vacía
  unión q q'                           = unión q' q
  unión q vacía                        = q
  unión q (poner v k q')               = poner v k (unión q q')

-- especificación como modelo: (conjuntos)
-- sea C = {(v1,k1),...,(vn,kn)} : ∀ i,j, i != j => ki != kj
  vacía                         = {}
  poner v' k' C                 = {(v',k')} U C  (Caso (_,k') ∉ C)
                                = C              (Caso (_,k') ∈ C)
  primero C                     = (v,k) : k = máx{k1,...,kn}
  sacar C                       = C \ {primero C}
  esVacía C                     = True  (Caso n = 0)
                                  False (Caso n > 0)
  unión C {(x1,y1),...,(xn,yn)} = C U {(xi,yi) : yi ∉ {k1,...,kn}}


-- (5)
  tad BalT (A:Ordered Set) where
    import Maybe
   |empty  : BalT A
   |join   : BalT A -> Maybe A -> BalT A -> BalT A
    size   : BalT A -> N
    expose : BalT A -> Maybe (BalT A, A, BalT A)

-- especificación algebráica:
  size empty = 0
  size (join l Nothing r)  = size l + size r
  size (join l (just x) r) = size l + size r + 1
  t = case expose t of
        Nothing       -> empty
        just(l, x ,r) -> join l just(x) r

  t = maybe (expose t) empty
        join (p1 (from just(expose t)))
             (just (p2 (from just(expose t))))
             (p3 (from just(expose t)))


-- (6)
  zip :: [a] -> [b] -> [(a,b)]
  zip [] ys         = []
  zip xs []         = []
  zip (x:xs) (y:ys) = (x,y) : zip xs ys
  
  unzip :: [(a,b)] -> ([a],[b])
  unzip []         = ([],[])
  unzip ((x,y):ps) = (x:xs, y:ys)
                     where (xs, ys) = unzip ps

{- Demostrar que ((uncurry zip) ◦ unzip) (ps) = id (ps)           -}

{- Definición de Inducción Estructural para [(a,b)] :             -}
{-  dada una propiedad P sobre [(a,b)],                           -}
{-  P(ps) vale ∀ ps::[(a,b)] si :                                 -}
{-  * P([])                                                       -}
{-  * P(ps) => P((x,y):ps)                                        -}

{- Sea p(ps) : ((uncurry zip) . unzip) (ps) = id (ps)             -}
{- Probamos p con Inducción Estructural sobre [(a,b)]             -}

-- Caso []
   ((uncurry zip) . unzip) []
-- = { def (.) }
   (uncurry zip (unzip []))
-- = { unzip 1 }
   uncurry zip ([], [])
-- = { uncurry }
   zip [] []
-- = { zip 1 }
   []
-- = { id }
   id []


-- Caso ((x,y):ps)
   ((uncurry zip) . unzip) ((x,y):ps)
-- = { def (.) }
   (uncurry zip (unzip ((x,y):ps)))
-- = { unzip 2 }
   uncurry zip (x:xs, y:ys) where (xs, ys) = unzip ps
-- = { uncurry }
   zip (x:xs) (y:ys) where (xs, ys) = unzip ps
-- = { zip 3 }
   (x,y) : zip xs ys where (xs, ys) = unzip ps
-- = { uncurry }
   (x,y) : uncurry zip (xs, ys) where (xs, ys) = unzip ps
-- = { where }
   (x,y) : (uncurry zip (unzip ps))
-- = { def (.) }
   (x,y) : ((uncurry zip) . unzip) (ps)
-- = { HI }
   (x,y) : id (ps)
-- = { id }
   (x,y):ps
-- = { id }
   id ((x,y):ps)


-- (7)
maxl :: [Int] -> Int
maxl []     = 0
maxl (x:xs) = max x (maxl xs)

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + (sum xs)

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + (length xs)

{- Demostrar que sum xs <= length xs * maxl xs.                   -}

{- Definición de Inducción Estructural para [a] :                 -}
{-  dada una propiedad P sobre [a], P(xs) vale ∀ xs::[a] si :  -}
{-  * P([])                                                       -}
{-  * P(xs) => P(x:xs)                                            -}

{- Sea p(xs) : sum xs <= length xs * maxl xs                      -}
{- Probamos p con Inducción Estructural sobre [a]                 -}

-- Caso []
   sum []
-- = { sum 1 }
   0
-- <= { aritmética }
   0 * 0
-- = { length 1, maxl 1 }
   length [] * maxl []


-- Caso (x:xs)                   {- lema 1 : ∀ a,b  -}
   sum (x:xs)                    {-   a <= max a b  -}
-- = { sum 2 }                   {-   b <= max a b  -}
   x + (sum xs)
-- <= { HI }
   x + (length xs) * (maxl xs)
-- <= { lema 1 }
   max x (maxl xs) + (length xs) * max x (maxl xs)
-- = { aritmética }
   (1 + (length xs)) * (max x (maxl xs))
-- = { length 2, maxl 2 }
   length (x:xs) * maxl (x:xs)


-- (8)
data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

-- a)
  size :: Arbol a -> Int
  size (Hoja _)       = 1
  size (Nodo _ t1 t2) = 1 + size t1 + size t2

-- b) ∀ Arbol a = t, ∃ k : size t = 2k+1

{- Definición de Inducción Estructural para [Arbol a] :           -}
{-  dada una propiedad P sobre [Arbol a],                         -}
{-  P(t) vale ∀ t::[Arbol a] si :                                 -}
{-  * P(Hoja a)                                                   -}
{-  * P(t1) y P(t2) => P(Nodo a t1 t2)                            -}

{- Sea p(t) : size t = 2k+1                                       -}
{- Probamos p con Inducción Estructural sobre [Arbol a]           -}

-- Caso (Hoja a)
   size (Hoja a)
-- = { size 1 }
   1
-- = { aritmética, k=0 }
   2k+1

-- Caso (Nodo a t1 t2)
   size (Nodo a t1 t2)
-- = { size 2 }
   1 + size t1 + size t2
-- = { HI }
   1 + 2(k1)+1 + 2(k2)+1
-- = { aritmética }
   1 + 2(k1) + 2(k2) + 2
-- = { aritmética }
   2(k1+k2+1) + 1
-- = { k = (k1) + (k2) + 1 }
   2k+1

-- c)
  mirror :: Arbol a -> Arbol a
  mirror (Hoja a)     = (Hoja a)
  mirror (Nodo a l r) = (Nodo a (mirror r) (mirror l))

-- d) (mirror ◦ mirror) = id

{- Definición de Inducción Estructural para [Arbol a] :           -}
{-  dada una propiedad P sobre [Arbol a],                         -}
{-  P(t) vale ∀ t::[Arbol a] si :                                 -}
{-  * P(Hoja a)                                                   -}
{-  * P(l) y P(r) => P(Nodo a l r)                                -}

{- Sea p(t) : (mirror . mirror) (t) = id (t)                      -}
{- Probamos p con Inducción Estructural sobre [Arbol a]           -}

-- Caso t = (Hoja a)
   (mirror . mirror) (Hoja a)
-- = { def (.) }
   mirror (mirror (Hoja a))
-- = { mirror 1 }
   mirror (Hoja a)
-- = { mirror 1 }
   (Hoja a)
-- = { id }
   id (Hoja a)


-- Caso t = (Node a l r)
   (mirror . mirror) (Node a l r)
-- = { def (.) }
   mirror (mirror (Node a l r))
-- = { mirror 2 }
   mirror (Node a (mirror r) (mirror l))
-- = { mirror 2 }
   (Node a (mirror (mirror l)) (mirror (mirror r)))
-- = { HI }
   (Node a id(l) id(r))
-- = { id }
   (Node a l r)
-- = { id }
   id (Node a l r)

-- e)
  hojas :: Arbol a -> Int
  hojas (Hoja a)       = 1
  hojas (Nodo a t1 t2) = (hojas t1) + (hojas t2)

  altura :: Arbol a -> Int
  altura (Hoja a)       = 1
  altura (Nodo a t1 t2) = 1 + max (altura t1) (altura t2)

{- Demostrar que ∀ Arbol a = t, hojas t < 2^(altura t)            -}

{- Definición de Inducción Estructural para [Arbol a] :           -}
{-  dada una propiedad P sobre [Arbol a],                         -}
{-  P(t) vale ∀ t::[Arbol a] si :                                 -}
{-  * P(Hoja a)                                                   -}
{-  * P(t1) y P(t2) => P(Nodo a t1 t2)                            -}

{- Sea p(t) : hojas t < 2^(altura t)                              -}
{- Probamos p con Inducción Estructural sobre [Arbol a]           -}

-- Caso t = (Hoja a)
   hojas (Hoja a)
-- = { hojas 1 }
   1
-- < { aritmética }
   2^(1)
-- = { altura 1 }
   2^(altura (Hoja a))

-- Caso t = (Nodo a t1 t2)
   hojas (Nodo a t1 t2)           {-  lema 1: ∀ a,b > 0   -}
-- = { hojas 2 }                  {-    a <= max a b      -}
   (hojas t1) + (hojas t2)}       {-    b <= max a b      -}
-- < { HI }
   2^(altura t1) + 2^(altura t2)
-- <= { lema 1 }
   2 * 2^(max (altura t1) (altura t2))
-- = { aritmética }
   2^(1 + max (altura t1) (altura t2))
-- = { altura 2 }
   2^(altura (Nodo a t1 t2))


-- (10)
  flatten :: Arbol a -> [a]
  flatten (Hoja a)     = [a]
  flatten (Node a l r) = flatten l ++ [a] ++ flatten r

  mapTree :: (a -> b) -> Arbol a -> Arbol b
  mapTree f (Hoja a)     = Hoja (f a)
  mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

{- Demostrar que (map f ◦ flatten) = (flatten ◦ mapTree f)        -}

{- Definición de Inducción Estructural para [Arbol a] :           -}
{-  dada una propiedad P sobre [Arbol a],                         -}
{-  P(t) vale ∀ t::[Arbol a] si :                                 -}
{-  * P(Hoja a)                                                   -}
{-  * P(l) y P(r) => P(Nodo a l r)                                -}

{- Sea p(t) : (map f . flatten) (t) = (flatten . mapTree f) (t)   -}
{- Probamos p con Inducción Estructural sobre [Arbol a]           -}

-- Caso t = (Hoja a)
   (map f . flatten) (Hoja a)
-- = { def (.) }
   map f (flatten (Hoja a))
-- = { flatten 1 }
   map f [a]
-- = { map 2 }
   [f a]
-- = { flatten 1 } 
   flatten (Hoja (f a))
-- = { mapTree 1 }
   flatten (mapTree f (Hoja a))
-- = { def (.) }
   (flatten . mapTree f) (Hoja a)

-- Caso t = (Node a l r)
   (map f . flatten) (Node a l r)
-- = { def (.) }                                {-            lema map:           -}
   map f (flatten (Node a l r))                 {-    map ([a] ++ [b] ++ [c])     -}
-- = { flatten 2 }                              {-                 =              -}
   map f ((flatten l) ++ [a] ++ (flatten r))    {- map [a] ++ map [b] ++ map [c]  -}
-- = { lema map }
   map f (flatten l) ++ map f [a] ++ map f (flatten r)
-- = { HI, map }
   flatten (mapTree f l) ++ [f a] ++ flatten (mapTree f r)
-- = { flatten 2 }
   flatten (Node (f a) (mapTree f l) (mapTree f r))
-- = { mapTree 2 }
   flatten (mapTree f (Node a l r))
-- = { def (.) }
   (flatten . mapTree f) (Node a l r)


-- (11)
  join :: [[a]] = [a]
  join []       = []
  join (xs:xss) = xs ++ join xss
 
  singleton :: a -> [a]
  singleton x = [x]

-- a) Demostrar que (join ◦ map singleton) = id

{- Definición de Inducción Estructural para [a] :                 -}
{-  dada una propiedad P sobre [a],                               -}
{-  P(xs) vale ∀ xs::[a] si :                                     -}
{-  * P([])                                                       -}
{-  * P(xs) => P(x:xs)                                            -}

{- Sea p(xs) : (join . map singleton) (xs) = id (xs)              -}
{- Probamos p con Inducción Estructural sobre [a]                 -}

-- Caso []
   (join . map singleton) []
-- = { def (.) }
   join (map singleton [])
-- = { map 1 }
   join []
-- = { join 1 }
   []
-- = { id }
   id []

-- Caso (x:xs)
   (join . map singleton) (x:xs)
-- = { def (.) }
   join (map singleton (x:xs))
-- = { map 2 }
   join (singleton x : map singleton xs)
-- = { join 2 }
   singleton x ++ join (map singleton xs)
-- = { singleton, def (.) }
   [x] ++ (join . map singleton) (xs)
-- = { HI }
   [x] ++ id (xs)
-- = { id }
   [x] ++ (xs)
-- = { def (++) }
   (x:xs)
-- = { id }
   id (x:xs)


-- b) Demostrar que (join ◦ map join) = (join ◦ join)

{- Definición de Inducción Estructural para [a] :                 -}
{-  dada una propiedad P sobre [a],                               -}
{-  P(xs) vale ∀ xs::[a] si :                                     -}
{-  * P([])                                                       -}
{-  * P(xs) => P(x:xs)                                            -}

{- Sea p(xs) : (join . map join) (xs) = (join . join) (xs)        -}
{- Probamos p con Inducción Estructural sobre [a]                 -}

-- Caso []
   (join . map join) []
-- = { def (.) }
   join (map join [])
-- = { map 1 }
   join []
-- = { join 1 }
   join (join [])
-- = { def (.) }
   (join . join) []

-- Caso (x:xs)
   (join . map join) (x:xs)
-- = { def (.) }
   join (map join (x:xs))
-- = { map 2 }
   join (join x : map join xs)
-- = { join 2 }
   join x ++ join (map join xs)
-- = { def (.) }
   join x ++ (join . map join) (xs)
-- = { HI }
   join x ++ (join . join) (xs)
-- 
   ...
--
   join (x ++ join xs)
--
   join (join (x:xs))
--
   (join . join) (x:xs)


-- (12)
