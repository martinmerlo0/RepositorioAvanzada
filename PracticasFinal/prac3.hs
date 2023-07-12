{-
.



7. Define la función que, dado un número natural, decida si el mismo es un
cuadrado perfecto o no.



11. Define la función compact, dada una lista retorna la lista sin los elementos
repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3]-}



{-1. Define una función que, dadas dos listas ys y xs de naturales ordenadas,
retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
elementos de ys y xs.-}


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
             where menores = [a | a <- xs , a < x]
                   mayores = [b | b <- xs , b >= x]



merge :: (Ord a) => [a] -> [a] -> [a]
merge xs ys = quicksort (xs ++ ys)




{-2. Define una función que, dada una lista de naturales, la ordene.-}


ordenar :: [Int] -> [Int]
ordenar xs = quicksort xs



{-
3. Define una función que, recursivamente y sólo utilizando adición y multipli-
cación, calcule, dado un natural n, el número 2n -}


potencia :: Int -> Int
potencia 0 = 1
potencia n = 2 * potencia (n-1)




{-4. Define una función que, dado un número natural n, retorne su representación
binaria como secuencia de bits.-}


binaria :: Int -> [Int]
binaria 0 = []
binaria n = mod n 2 : binaria (div n 2)


binaria' :: Int -> [Int]
binaria' n = reverse (binaria n)



{-5 *. Define una función que, dado un número natural n en su representación
binaria, decida si n es par o no.-}


parbin :: [Int] -> Bool
parbin xs = last xs == 0 


{-6. Define la función que retorne la distancia de Hamming: dadas dos listas es el
número de posiciones en que los correspondientes elementos son distintos. Por
ejemplo: distanciaH ”roma””camino”− > 3
distanciaH ”romano””rama”− > 1-}



distanciaH :: String -> String -> Int
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys) | x /= y = 1 + distanciaH xs ys 
                         | otherwise = distanciaH xs ys 





{-8. Define la función repetidos de forma tal que dado un elemento z y un entero
n; z aparece n veces.-}


repetidos :: a -> Int -> [a]
repetidos z 0 = []
repetidos z n = z : repetidos z (n-1)



{-9. Define la función nelem tal que nelem xs n es elemento enésimo de xs,
empezando a numerar desde el 0. Por ejemplo:
nelem [1, 3, 2, 4, 9, 7] 3 −> 4  -}



nelem :: [a] -> Int -> a 
nelem [] _ = error "Lista vacia"
nelem (x:xs) 0 = x
nelem (x:xs) n = nelem xs (n-1)


{-10 *. Define la función posicionesC tal que posicionesC xs c es la lista de la
posiciones del caracter c en la cadena xs. Por ejemplo:
posicionesC ”Catamarca”0 a0 − > [1, 3, 5, 8]-}


posicionesC :: String -> Char -> Int -> [Int]
posicionesC [] _ _ = []
posicionesC (x:xs) c n | x == c = n : posicionesC xs c (n + 1)
                       | otherwise = posicionesC xs c (n+1)