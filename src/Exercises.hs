{-
TP 1:
1. invertir una lista
2. sumar todos los elementos de una lista
3. obtener el mayor elemento de una lista
4. implementar la función de fibonacci
5. permutar los valores de una lista
6. ordernar una lista de tuplas a partir del primer elemento de la tupla
7. determinar si una lista es capicua
8. insertar el valor x en la posición i de una lista
9. calcular el tamaño de una lista
10. determinar cuantas veces se repite el valor x en una lista
-}

-- 1. invertir una lista
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- 2. sumar todos los elementos de una lista
sum' :: (Num i) => [i] -> i
sum' = foldl (+) 0

-- 3. obtener el mayor elemento de una lista
max' :: (Num i, Ord i) => [i] -> i
max' [] = 0
max' (x:xs)
    | x > max' xs = x
    | otherwise = max' xs

-- 4. implementar la función de fibonacci
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

-- 5. permutar los valores de una lista
permutation :: [a] -> [a]
permutation [] = []
permutation (x:y:xs) = [y] ++ [x] ++ permutation xs

-- 6. ordernar una lista de tuplas a partir del primer elemento de la tupla
sortTuple :: (Ord a) => [(a,b)] -> [(a,b)]
sortTuple [] = []
sortTuple (x:xs) =
    let smallerThan = [y | y <- xs, fst y <= fst x]
        greaterThan = [y | y <- xs, fst y > fst x]
    in  (sortTuple smallerThan) ++ [x] ++ (sortTuple greaterThan)

-- 7. determinar si una lista es capicua
polindrome :: (Eq a) => [a] -> Bool
polindrome [] = True
polindrome (x:xs)
    | x == last xs = polindrome $ init xs
    | otherwise = False

-- 8. insertar el valor x en la posición i de una lista
insertElement :: [a] -> a -> Int -> [a]
insertElement [] _ _ = []
insertElement (x:xs) y 1 = [y] ++ [x] ++ xs
insertElement (x:xs) y i = [x] ++ insertElement (xs) y (i-1)

-- 9. calcular el tamaño de una lista
listSize :: (Integral i) => [a] -> i
listSize = sum . map (\_ -> 1)


-- 10. determinar cuantas veces se repite el valor 'a' en una lista
appearances :: (Eq a, Integral i) => a -> [a] -> i
appearances x xs = listSize $ filter (\y -> y == x) xs

-- QUICKSORT 1
quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) =
    let smallerThan = [a | a <- xs, a <= x]
        greaterThan = [a | a <- xs, a > x]
    in  (quicksort1 smallerThan) ++ [x] ++ (quicksort1 greaterThan)

-- QUICKSORT 2
quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x:xs) =
    let smallerThan = quicksort2 (filter (<=x) xs)
        greaterThan = quicksort2 (filter (>x) xs)
    in smallerThan ++ [x] ++ greaterThan


-- higher order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (\x y -> (x,y)) -- crea una función parcial de zipWith' donde ya le pasa la función y espera las 2 listas

-- lambdas: \argument argument2 ... -> functionBody
    -- \x y -> x + y

-- fold: recorre una lista de izq a der o al revés, y aplica la función que le pasen.
    -- requiere un valor inicial b
        -- foldr (b -> a -> b) -> b -> [a] -> b
foldr' :: (b -> a -> b) -> b -> [a] -> b
foldr' _ y [] = y
foldr' f y [x] = f y x
foldr' f y (x:xs) = foldr' f (f y x) xs

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

--1. Implementar la suma binaria
data Bit = Zero | One deriving (Show, Eq)
type Binary = [Bit]
data SumResult = SumResult {carry::Bit, result::Bit} deriving (Show, Eq)

bitsum :: Bit -> Bit -> SumResult
bitsum Zero Zero = SumResult Zero Zero
bitsum One One = SumResult One One
bitsum _ _  = SumResult Zero One

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

treeInsert :: (Ord a) => a -> Tree a -> Bool
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x < a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem left
    | x > a  = treeElem right

treeDepth :: (Ord a) => Tree a -> Int
treeDepth t = treeDepth' t 0

treeDepth' :: (Ord a) => Tree a -> Int -> Int
treeDepth' EmptyTree acc = acc
treeDepth' (Node _ left right) acc = max (treeDepth' left (acc+1)) (treeDepth' right (acc+1))