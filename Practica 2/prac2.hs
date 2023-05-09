--Retorna la cabeza de la lista 
--EJerc 2

hd :: [a] -> a
--hd [] = 
hd (x:xs) = x


--retorna la lista sin la cabeza

tl :: [a] -> [a]
tl (x:xs) = xs 



--last retorna el ultimo elemento de la lista 


last' :: [a] -> a 
last' [x] = x
last' (x:xs) = last' xs 


-- retorna todos los elementos menos el ultimo 


init' :: [a] -> [a]
init' [x] = [] 
init' (x:xs) = x : init'(xs)

--Ejerc 3
maxDos :: Int -> Int -> Int
maxDos x y = if x > y then x else y 


maxTres :: Int -> Int -> Int -> Int 
maxTres x y z = maxDos x (maxDos y z )


--EJerc 4

conc :: [a] -> [a] -> [a]
conc [] [] = []
conc [] ys = ys
conc xs [] = xs
conc (x:xs) ys = x : conc xs ys 


tirar :: [a] -> Int -> [a]
tirar [] n = []
tirar (x:xs) 0 = (x:xs)
tirar (x:xs) n = tirar xs (n-1) 



tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 xs = [] 
tomar n (x:xs) = x : tomar (n-1) xs

concFinal :: [a] -> a -> [a]
concFinal [] n = [n]
concFinal [x] n = [x,n]
concFinal (x:xs) n = x : concFinal xs n 













--5. Defina una funci ́on abs: Int -> Int que calcula el valor absoluto de un
--n ́umero.


absoluto :: Integer -> Integer 
absoluto n = if n >= 0 then n else (-1)*(n)

--EJerc 6
edad :: (Int, Int, Int) -> (Int , Int , Int) -> Int
edad (d,m,a) (d2,m2,a2)
    | d == d2 && m == m2 = abs(a-a2)
    | d < d2 && m == m2 = abs(a-a2)
    | d > d2 && m == m2 = abs(a-a2) -1
    | m > m2 = abs(a-a2) -1
    | m < m2 = abs(a-a2)
    | otherwise = abs(a-a2)


--7. La disyunci ́on excluyente xor de dos f ́ormulas se verifica si una es verdadera
--y la otra es falsa. Defina la funci ́on xor que calcule la disyunci ́on excluyente a
--partir de la tabla de verdad.
--Ahora defina la funci ́on xor2 que calcule la disyunci ́on excluyente pero sin
--que considere todos los posibles valores de las entradas. Cu ́al ser ́a la diferencias
--entre ambas definiciones?

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True 
xor False True = True 
xor False False = False

xor2 :: Bool -> Bool -> Bool 
xor2 True True = False
xor2 False False = False 
xor2 _ _ = True 

--8. Defina una funci ́on que dado un n ́umero natural, decida si el mismo es primo o no.





divisores :: Int -> [Int]
divisores n = [y | y <- [1..n] , (mod n y) == 0]



primo :: Int -> Bool
primo n = if length (divisores n) == 2 then True else False

--Ejerc 9




--9 *. Defina una funci´on que dado un n´umero natural n, retorne la lista de todos
--los n´umeros naturales primos menores que n.


primosmenores :: Int -> [Int]
primosmenores n = [x | x <-[1..n], primo x < primo n]





--10. Defina una funci ́on que dada una lista, retorne la reversa de la misma.






reversa :: [a] -> [a]
reversa [] = []
reversa [x] = [x]
reversa (x:xs) = conc (reversa xs) [x]


--11. Defina una funci ́on que dadas dos listas, decida si las listas son iguales.



iguales ::(Eq a) => [a] -> [a] -> Bool 
iguales [] [] = True
iguales [] ys = False
iguales xs [] = False 
iguales (x:xs) (y:ys) = ((x == y) && iguales xs ys) 


--12 *. Defina una funci ́on que dada una lista decida si es un pal ́ındromo o no.



--Aca rehuso las funciones creadas anteriormente para facilitar los calculos

pali :: (Eq a) =>[a] -> Bool
pali [] = True
pali [x] = True
pali (x:xs) = ((x:xs) == reversa (x:xs))





--Ejerc 13

discriminante :: Float -> Float -> Float -> Float
discriminante a b c = b^2-4*a*c


raices :: Float -> Float -> Float -> Int 
raices a b c
    | discriminante a b c > 0 = 2
    | discriminante a b c == 0 = 1
    | otherwise   = 0


