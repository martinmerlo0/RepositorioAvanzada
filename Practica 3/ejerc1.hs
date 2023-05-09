{-1. Define una funci ́on que, dadas dos listas ys y xs de naturales ordenadas,
retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
elementos de ys y xs.-}



ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = 
    ordenaRapida menores ++ [x] ++ ordenaRapida mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y >  x]



conc :: [a] -> [a] -> [a]
conc [] [] = []
conc xs [] = xs
conc [] ys = ys
conc (x:xs) ys = x : conc xs ys 



merge :: [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = conc xs []
merge [] ys = conc ys []
merge xs ys = conc xs ys