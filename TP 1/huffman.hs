import Data.Map as M
import Heap as H
-- ************************************************************************* --

-- ************************************************************************* --

-- 1) --

data HTree = Leaf Char Int | Node HTree HTree Int deriving (Show)

instance Eq HTree where
    (Leaf c1 n1) == (Leaf c2 n2) = (c1 == c2) && (n1 == n2)
    (Node left1 right1 n1) == (Node left2 right2 n2) = (left1 == left2) && (right1 == right2) && (n1 == n2) 
    _ == _ = False

instance Ord HTree where
    compare (Leaf _ n1) (Leaf _ n2) = compare n1 n2
    compare (Leaf _ n1) (Node _ _ n2) = compare n1 n2
    compare (Node _ _ n1) (Leaf _ n2) = compare n1 n2
    compare (Node _ _ n1) (Node _ _ n2) = compare n1 n2 

type FreqMap = Map Char Int

-- 2) --

buildFreqMap :: String -> FreqMap
buildFreqMap "" = fromList []
buildFreqMap (c:cs) = M.insertWith (+) c 1 (buildFreqMap cs)

-- 3) --

weight :: HTree -> Int
weight (Leaf _ n) = n
weight (Node _ _ n) = n
 
buildHeap :: [(Char,Int)] -> H.Heap HTree
buildHeap [] = H.empty
buildHeap ((k,a):ms) = H.insert (Leaf k a) (buildHeap ms)

buildHTree' :: H.Heap HTree -> HTree
buildHTree' h = if H.isEmpty (H.deleteMin h) then (H.findMin h)
    else
        buildHTree' (H.insert (Node x' y' ((weight x') + (weight y'))) y) 
            where x = (H.deleteMin h)
                  y = (H.deleteMin x)
                  x' = H.findMin h
                  y' = H.findMin x

buildHTree :: FreqMap -> HTree
buildHTree m = buildHTree' (buildHeap (M.toList m))

-- 4) --

data Bit = Zero | One deriving (Eq,Show)
type Code = [Bit]
type CodeMap = Map Char Code

buildCodeMap' :: HTree -> Code -> CodeMap
buildCodeMap' (Leaf c n) s = M.insertWith (++) c s M.empty
buildCodeMap' (Node l r w) s = M.union (buildCodeMap' l (s++[Zero])) ((buildCodeMap' r (s++[One])))

buildCodeMap :: HTree -> CodeMap
buildCodeMap t = buildCodeMap' t []

-- 5) --

look :: Maybe Code -> Code
look Nothing = error "(!!!) Caracter no encontrado (!!!)"
look (Just a) = a

encode :: CodeMap -> String -> Code
encode m "" = []
encode m (c:cs) = (look (M.lookup c m)) ++ (encode m cs)

-- 6) --

decode' :: HTree -> HTree -> Code -> String
decode' t (Leaf c _) [] = [c]
decode' t (Leaf c _) b = c:(decode' t t b)
decode' t (Node l r _) (b:bs) = if b == Zero then (decode' t l bs) else (decode' t r bs)

decode :: HTree -> Code -> String
decode t c = decode' t t c