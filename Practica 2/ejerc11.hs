--11. Defina una funci ́on que dadas dos listas, decida si las listas son iguales.



iguales ::(Eq a) => [a] -> [a] -> Bool 
iguales [] [] = True
iguales [] ys = False
iguales xs [] = False 
iguales (x:xs) (y:ys) = ((x == y) && iguales xs ys) 