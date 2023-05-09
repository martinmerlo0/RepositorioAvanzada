--12 *. Defina una funci ́on que dada una lista decida si es un pal ́ındromo o no.

conc :: [a] -> [a] -> [a]
conc [] [] = []
conc [] ys = ys
conc xs [] = xs
conc (x:xs) ys = x : conc xs ys 


reversa :: [a] -> [a]
reversa [] = []
reversa [x] = [x]
reversa (x:xs) = conc (reversa xs) [x]


--Aca rehuso las funciones creadas anteriormente para facilitar los calculos

pali :: (Eq a) =>[a] -> Bool
pali [] = True
pali [x] = True
pali (x:xs) = ((x:xs) == reversa (x:xs))