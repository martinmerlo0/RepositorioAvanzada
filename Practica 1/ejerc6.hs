--6. Utilizando la funci ́on sum1
--
--, la funci ́on mod y un n ́umero representado de
--igual manera que en el [ item 5 ] determine si un n ́umero es m ́ultiplo de 3.


multiplo3' :: [Int] -> Bool
multiplo3' xs = (mod (sum xs) 3 == 0)