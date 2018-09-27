-- ordenar una lista de elementos comparables:
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = lesserSorted ++ [x] ++ higherSorted
    where
        lesserSorted = filter (<=x) xs
        higherSorted = filter (>x) xs

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

hasElement :: (Ord a) => Tree a -> a -> Bool
hasElement Nil _ = False
hasElement (Node x left right) e
    | x == e = True
    | x <  e = hasElement left e
    | otherwise = hasElement right e
