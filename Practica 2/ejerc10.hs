--10. Defina una funci ́on que dada una lista, retorne la reversa de la misma.

concFinal :: [a] -> a -> [a]
concFinal [] n = [n]
concFinal [x] n = [x,n]
concFinal (x:xs) n = x : concFinal xs n 

conc :: [a] -> [a] -> [a]
conc [] [] = []
conc [] ys = ys
conc xs [] = xs
conc (x:xs) ys = x : conc xs ys 


reversa :: [a] -> [a]
reversa [] = []
reversa [x] = [x]
reversa (x:xs) = conc (reversa xs) [x]