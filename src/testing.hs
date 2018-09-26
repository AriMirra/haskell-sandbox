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
max' :: (Num i, Ord i) => [i] -> i
max' [] = 0
max' (x:xs)
    | x > max' xs = x
    | otherwise = max' xs


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
sortTuple :: (Ord a) => [(a,b)] -> [(a,b)]
sortTuple [] = []
sortTuple (x:xs) = sortTuple [y | y <- xs, fst x > fst y] ++ [x] ++ sortTuple [y | y <- xs, fst x <= fst y]

-- 7. determinar si una lista es capicua
polindrome :: (Eq a) => [a] -> Bool
polindrome [] = True
polindrome (x:xs)
    | x == last xs = polindrome (init xs)
    | otherwise = False

-- 8. insertar el valor x en la posición i de una lista
insertElement :: [a] -> a -> Int -> [a]
insertElement [] _ _ = []
insertElement (x:xs) y 1 = [y] ++ [x] ++ xs
insertElement (x:xs) y i = [x] ++ insertElement (x:xs) y (i-1)

-- 9. calcular el tamaño de una lista
listSize :: (Integral i) => [a] -> i
listSize [] = 0
listSize (x:xs) = 1 + listSize xs

-- 10. determinar cuantas veces se repite el valor 'a' en una lista
repeatElement :: (Eq a) => [a] -> a -> Int
repeatElement [] _ = 0
repeatElement (x:xs) y
    | x == y = 1 + z
    | otherwise = z
    where z = repeatElement xs y

    
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
    let smallerThan = quicksort2 $ filter (<=x) xs
        greaterThan = quicksort2 $ filter (> x) xs
    in  smallerThan ++ [x] ++ greaterThan