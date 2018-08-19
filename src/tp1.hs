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
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 2. sumar todos los elementos de una lista
sum' :: (Num i) => [i] -> i
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 3. obtener el mayor elemento de una lista
-- max' :: (Ord i) => [i] -> i
-- max' [] = error "empty list"
-- max' (x:xs)
--     | x > max' xs = x
--     | otherwise = max' xs

-- 4. implementar la función de fibonacci
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci i = (fibonacci (i-1)) + (fibonacci (i-2))

-- 5. permutar los valores de una lista
permutation :: [a] -> [a]
permutation [] = []
permutation (x:y:xs) = [y] ++ [x] ++ permutation xs

-- 6. ordernar una lista de tuplas a partir del primer elemento de la tupla
-- sortTuple :: (Ord a) => [(a,b)] -> [(a,b)]
-- sortTuple [] = []
-- sortTuple [(x,y)] 

-- 7. determinar si una lista es capicua


-- 8. insertar el valor x en la posición i de una lista
-- insertInList :: (Ord x, Integral i) => x -> i -> [x] -> [x]
-- insertInList _ _ [] = error "empty list"
-- insertInList x 1 [x] = x:[x]
-- insertInList x i (x:xs) = ???

-- 9. calcular el tamaño de una lista
listSize :: (Integral i) => [a] -> i
listSize [] = 0
listSize (x:xs) = 1 + listSize xs

-- 10. determinar cuantas veces se repite el valor 'a' en una lista
appearances :: (Eq a, Integral i) => a -> [a] -> i
appearances _ [] = 0
appearances i (x:xs) 
    | x == i = 1 + appearances i xs
    | otherwise = appearances i xs


-- QUICKSORT 1
quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x: xs) =
    let smallerThan = [a | a <- xs, a <= x]
        greaterThan = [a | a <- xs, a > x]
    in  smallerThan ++ [x] ++ greaterThan

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

sum'' :: (Num a) => [a] -> a
sum'' [] = 0
sum'' x = foldr (\x y -> x + y) 0 x
