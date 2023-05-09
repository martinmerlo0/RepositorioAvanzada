--2. Define una funci ́on que, dada una lista de naturales, la ordene.


ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = 
    ordenaRapida menores ++ [x] ++ ordenaRapida mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y >  x]
