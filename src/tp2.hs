-- para cambiar la precedencia y no usar paréntesis, puedo usar el signo $ y primero se ejecuta lo de la derecha del signo, y luego la izquierda
-- f (g [...]) == f $ g [...]

{- data ; type ; alias
    type le da un sobrenombre a un tipo de dato que ya existe, para mayor legibilidad
    data define un tipo de dato que no existe
type String = [Char] ; type Graph = [(a,[a])]
data Tree = Leaf a | Branch a Tree Tree | Nil
    instance Eq Tree (Eq a) => a where
        (==) Branch x lf rf Branch y ls rs
            | x == y = lf == ls && rf == rs
            | otherwise = False
        (==) Leaf x Branch _ _ _ = False
        (==) Branch _ _ _ Leaf x = False

data vs aliases
    data Person = Person String String
        firstName Person a b = a
        lastName Person a b = b
    data Person = Person {firstName :: String, lastName :: String} deriving (Eq)  
    // deriving te ahorra de implementar TypeClasses, Eq compararía todos los atributos en el orden que fueron expresados
    // si quiero una función de comparación particular la tendría que implementar yo
        instance Eq Person where
                (==) x y = x:DNI == y:DNI
        firstName Person = Person : firstName
        lastName p = p : firstName

data Maybe a = Nothing | Just a
divide :: (Num a) => a -> a -> Maybe a
divide _ 0 = Nothing
divide a b = Just a/b
-}

{-
TP2:
1. Implementar la suma binaria
2. Definir un tipo arbol (Tree) y definir una función de construcción
3. Dado el tipo Graph [(Nodo, [Nodo Vecino])], determinar si existen ciclos
4. Dado el tipo Graph de 3, determinar  si existe camino entre dos nodos
5. Deginir una funciÛn que retorne los primeros N n˙meros primos
6. Resolver el problema de las 8 reinas
7. Implementar el algoritmo de compresiÛn de Huffman
8. Dado el tipo de tree del punto 2, determinar la profundidad del ·rbol
9. Implementar una lista de contactos (Contact {name::String, Phone::Int, email::String}), funciones add, find por name, y email
10. Dada una matriz obtener la transpuesta
-}

data Bit = Zero | One deriving Eq
-- instance Show Bit where 
--     Zero = "0"
--     One = "1"

type Binary = [Bit]

-- bitsum :: Bit -> Bit  -> (Bit, Bit)
-- bitsum Zero Zero = (_,_) 
-- bitsum One One = (_,_) 
-- bitsum _ _ = (_,_) 


