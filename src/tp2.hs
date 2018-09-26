{-
TP2:
1. Implementar la suma binaria
2. Definir un tipo arbol (Tree) y definir una función de construcción
3. Dado el tipo Graph [(Nodo, [Nodo Vecino])], determinar si existen ciclos
4. Dado el tipo Graph de 3, determinar  si existe camino entre dos nodos
5. Deginir una función que retorne los primeros N números primos
6. Resolver el problema de las 8 reinas
7. Implementar el algoritmo de compresiÛn de Huffman
8. Dado el tipo de tree del punto 2, determinar la profundidad del ·rbol
9. Implementar una lista de contactos 
(Contact {name::String, Phone::Int, email::String}), funciones add, find por name, y email
10. Dada una matriz obtener la transpuesta
-}
import Data.List (nub)

data Bit = Zero | One deriving (Show, Eq)
type Binary = [Bit]
data SumResult = SumResult{carry::Bit, result::Bit} deriving Eq

bitsum :: Bit -> Bit  -> SumResult
bitsum Zero Zero = SumResult Zero Zero
bitsum One One = SumResult One Zero
bitsum _ _ = SumResult Zero One

bitAdd :: Bit -> Bit -> Bit -> Binary
bitAdd x y z = [result carryAdd, result secondAdd]
    where
        firstAdd = bitsum x y
        secondAdd = bitsum (result firstAdd) z
        carryAdd = bitsum (carry firstAdd) (carry secondAdd)


-- 2. Definir un tipo arbol (Tree) y definir una función de construcción
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right



-- 4. Dado el tipo Graph de 3, determinar  si existe camino entre dos nodos
data GraphNode a = GraphNode a
type Graph a = [(GraphNode a, [GraphNode a])]

neighbors :: (Eq a) => (Graph a) -> a -> [a]
neighbors [] _ = []
neighbors g a = foldr(\x y -> x ++ y) [] [(snd t) | t <- g, (fst t) == a] 
-- list: quiero el segundo elemento de t, t toma del grafo donde el primer elemento de t == a
    -- devuelve un solo elemento

connected :: (Eq a) => (Graph a) -> a -> a -> Bool
connected g f t = connectedV g [] f t

connectedV :: (Eq a) => (Graph a) -> [a] -> a -> a -> Bool
connectedV g v f t
    | foldr (\r s -> r || s) False [z == t | z <- n] = True -- True si t está en los vecinos del grafo f
    | foldr (\r s -> r || s) False [z == f | z <- v] = False
    | otherwise = foldr (\r s -> r || s) False [connectedV g (v ++ [f]) z t | z <- n]
    where n = neighbors g f

-- f = from, t = to, g = graph, v = array de grafos vecinos


-- huffman

data HuffmanTree a = HuffmanNode a Int (HuffmanTree a) (HuffmanTree a) | Nil

reps :: (Eq a) => [a] -> a -> Int
reps [] _ = 0
reps (x:xs) y
        | x == y = 1 + reps xs y
        | otherwise = reps xs y

-- qty :: [a] -> [(a, Int)]

