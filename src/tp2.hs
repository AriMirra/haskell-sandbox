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
-- 8. Dado el tipo de tree del punto 2, determinar la profundidad del ·rbol
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

treeDepth :: (Ord a) => Tree a -> Int
treeDepth t = treeDepth' t 0

treeDepth' :: (Ord a) => Tree a -> Int -> Int
treeDepth' EmptyTree depth = depth
treeDepth' (Node _ left right) depth = max (treeDepth' left (depth + 1)) (treeDepth' right (depth + 1))


-- 5. Deginir una función que retorne los primeros N números primos
isPrime :: Int -> Bool
isPrime n = isPrime' n 2

isPrime' :: Int -> Int -> Bool
isPrime' n current
    | current <= (floor $ sqrt $ fromIntegral n) = (n `mod` current) /= 0 && isPrime' n (current + 1)
    | otherwise = True

generatePrimes :: Int -> [Int]
generatePrimes n = take n [i | i <- [2..], isPrime i]

-- 9. Implementar una lista de contactos 
data Contact = Contact {name::String, phone::Int, email::String}
type Agenda = [Contact]

addContact :: Agenda -> Contact -> Agenda
addContact a c = c : a

findByName :: Agenda -> String -> Maybe Contact
findByName [] _ = Nothing
findByName (c:cs) n
    | n == name c = Just c
    | otherwise = findByName cs n

findByEmail :: Agenda -> String -> Maybe Contact
findByEmail [] _ = Nothing
findByEmail (c:cs) e 
    | e == email c = Just c
    | otherwise = findByEmail cs e

-- 10. Dada una matriz obtener la transpuesta
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose m = map head m : (transpose $ map tail m)

