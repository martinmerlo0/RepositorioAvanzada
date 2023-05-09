{-

6 *. Defina una funci ́on edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int
que dada dos fechas indica los a ̃nos transcurridos entre ellas. Por ejemplo:

edad (20,10,1968) (30,4,1987) = 18



8. Defina una funci ́on que dado un n ́umero natural, decida si el mismo es primo
o no.

9 *. Defina una funci ́on que dado un n ́umero natural n, retorne la lista de todos
los n ́umeros naturales primos menores que n.



13. Defina una funci ́on que dados tres n ́umeros a, b, c devuelva la cantidad de
ra ́ıces reales de la ecuaci ́on ax2 + bx + c = 0
-}


--13. Defina una funci ́on que dados tres n ́umeros a, b, c devuelva la cantidad de
--ra ́ıces reales de la ecuaci ́on ax2 + bx + c = 0



discriminante :: Float -> Float -> Float -> Float
discriminante a b c = b^2-4*a*c


raices :: Float -> Float -> Float -> Int 
raices a b c
    | discriminante a b c > 0 = 2
    | discriminante a b c == 0 = 1
    | otherwise   = 0






--edad (20,10,1968) (30,4,1987) = 18


edad :: (Int, Int, Int) -> (Int , Int , Int) -> Int
edad (d,m,a) (d2,m2,a2)
    | d == d2 && m == m2 = abs(a-a2)
    | d < d2 && m == m2 = abs(a-a2)
    | d > d2 && m == m2 = abs(a-a2) -1
    | m > m2 = abs(a-a2) -1
    | m < m2 = abs(a-a2)
    | otherwise = abs(a-a2)