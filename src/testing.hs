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
reverse' (x:xs) = reverse' xs ++ [x]

-- 2. sumar todos los elementos de una lista
sum' :: (Num i) => [i] -> i
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 3. obtener el mayor elemento de una lista
max' :: (Ord i) => [i] -> i
max' [] = 0
max' (x:xs)
    | x > max' xs = x
    | otherwise = max' xs


-- 4. implementar la función de fibonacci
fibonacci :: (Integral a) => a -> a


-- 5. permutar los valores de una lista
permutation :: [a] -> [a]


-- 6. ordernar una lista de tuplas a partir del primer elemento de la tupla
sortTuple :: (Ord a) => [(a,b)] -> [(a,b)]


-- 7. determinar si una lista es capicua


-- 8. insertar el valor x en la posición i de una lista
insertInList :: (Ord x, Integral i) => x -> i -> [x] -> [x]


-- 9. calcular el tamaño de una lista
listSize :: (Integral i) => [a] -> i


-- 10. determinar cuantas veces se repite el valor 'a' en una lista
appearances :: (Eq a, Integral i) => a -> [a] -> i



-- QUICKSORT 1
quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []


-- QUICKSORT 2
quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
