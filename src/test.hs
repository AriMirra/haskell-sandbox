{-
TP:
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

-- 1
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 2
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

--3
max' :: (Ord a) => [a] -> a
max' [] = error "empty list"
max' [x] = x
max' (x:xs)
    | x > m = x
    | otherwise = m
    where m = max' xs

-- 4
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci i = (fibonacci (i-1)) + (fibonacci (i-2))

-- 5
permutation :: [a] -> [a]
permutation [] = []
permutation (x:y:xs) = [y] ++ [x] ++ permutation xs

-- 6
sortTuple :: (Ord a) => [(a,b)] -> [(a,b)]
sortTuple [] = []
sortTuple [(x,y)] 