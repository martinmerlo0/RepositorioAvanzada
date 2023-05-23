-- | Calcula todos los segmentos iniciales

initSeg :: [a] -> [[a]]
initSeg xs = [as | (as,bs) <- split2 xs]

finalSeg :: [a] -> [[a]]
finalSeg xs = []:[cs | (as,bs,cs) <- split3 xs, length cs > 0]

-- | Retorna todos los subsegmentos de una lista utilizando split3

subSeg :: [a] -> [[a]]
subSeg xs = [] : [bs | (as,bs,cs) <- split3 xs , length bs > 0]



split2 :: [a] -> [([a],[a])]
split2 xs = [(take i xs, drop i xs) | i <-[0..length xs]]




split3 :: [a] -> [([a],[a],[a])]
split3 xs = [(as,bs,cs) | (as,ys) <- split2 xs, (bs,cs) <- split2 ys]


{-
Ejercicio 5. Dadas las funciones split3 : [a]− > [([a], [a], [a])] y split2 : [a]− >
[([a], [a])] dadas en clases. Escribir las siguientes especificaciones usando listas
por comprensi ́on:
• La especificaci ́on del ejercicio 1(e).
• La especificaci ́on del ejercicio 3(c).
• La especificaci ́on del ejercicio 3(d).
• La siguiente especificaci ́on: Dada una lista de n ́umeros, calcular el valor
de subsegmento de suma m ́axima.


• f.xs.ys determina si ys es una subsecuencia de xs.-}

f :: (Eq a) =>[a] -> [a] -> Bool
f xs ys = elem ys (subSeg xs)



secFinal :: (Eq a) => [a] -> [a] -> Bool
secFinal xs ys = elem ys (finalSeg xs)



{--
La siguiente especificaci ́on: Dada una lista de n ́umeros, calcular el valor
de subsegmento de suma maxima.-}


sumaMaxima :: [Int] -> Int
sumaMaxima xs = maximum [sum bs | (as,bs,cs) <- split3 xs, length bs > 0]