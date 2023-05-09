{-Ejercicios Repaso
---------------------------------------

---------------------------------------------

----------------------------------------------------   

------------------------------------------------------  

----------------------------------------------
-}

--1) a) En Haskell, definir una función

--f : Int -> [Int] -> Bool  
 
--que dado un número n y una lista xs, retorne true si y solo si hay repetidos en los primeros n elementos de la lista xs.




{-b) Utilice su número de documento para evaluar:
f 3 <su dni> 
con evaluación aplicativa y con evaluación normal. 
c) Qué sucede si evalúa f  3 [1..] con evaluación aplicativa, y que sucede si la evalúa usando orden normal.-}


{-Usando orden normal me duvuelve Falso ya que la lista infinita de esa forma devuelve los numeros de forma creciente es decir 
1,2,3,4,5,6,7,8 .... N 


es decir repetidos 3 [1..]
va a intentar reducir la expresion mas a la izq y mas adentro que en este caso es la lista infinita
por lo tanto se va a quedar computando eso y nunca va a devolver un valor 


con forma normal lo que pasaria es que entra a la definicion de repetidos y obtiene el take 3 de la lista infinita
por lo tanto se queda con la lista [1,2,3]
y al comparar si en esos elementos alguno se repite la funcion devolveria falso-}



quicksort :: (Ord a)=>[a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
            where menores = [y | y <- xs , y < x]
                  mayores = [a | a <- xs , a >= x]


iguales :: (Eq a) => [a] -> Bool
iguales [] = False
iguales [_] = False 
iguales (x:y:ys) = x == y || iguales (y:ys)  


repetidos ::(Eq a,Ord a)=> Int -> [a] -> Bool
repetidos n xs = iguales (quicksort (take n xs))


aux' :: (Eq a ) => a ->[a]->Bool
aux' _ [] = False
aux' x [y] = x == y
aux' x (y:ys) = x == y || aux' x ys 





--[1,2,3,4,4,5]




{-2) Dada las siguientes definiciones 
K.x.y = x
inf = inf+1
a) Evaluar normal y aplicativo la siguiente expresión:


Forma aplicativa

K.3.inf 

k.3.(inf+1)
k.3.(inf+1+1)
k.3.(inf+1+1+1)
k.3.(inf+1+1+1+1) y asi indefinidamente ya que inf no tiene forma normal
y la forma aplicativa empezaria intentando reducir inf  

   
   
   
Forma normal

k.3.inf

=

3

puesto que agarra la definicion de K
y K lo que hace es retornar el primer parametro que en este caso
es 3
   
   
   
  -}




{-3)
  a)Definir en haskell un tipo nuevo para árboles binarios, que distinga las hojas.
  b) Definir además una función que calcule la cantidad de hojas de un árbol.-}






{-4) En haskell definir con listas por comprensión, una lista que contenga todas las coordenadas de una Matriz infinita, pero de manera Diagonal.
Es decir : 
darCoordDiagonalmente = 
[(0,0),
(0,1),(1,0),
(0,2),(1,1),(2,0),
(0,3),(1,2),(2,1),(3,0),
(0,4),(1,3),(2,2),(3,1),(4,0),
(0,5),(1,4),(2,3),(3,2),(4,1),(5,0),
(0,6),(1,5),(2,4),(3,3),(4,2),(5,1),(6,0)
.. ]-}

diag :: Int -> [(Int,Int)]
diag n = [(n-i,i) | i <-[0..n]]

allpares :: Int -> [(Int,Int)]
allpares n = concatt [diag n | n <-[0..n] ]


concatt :: [[a]] -> [a]
concatt [] = []
concatt (xs:xss) = xs ++ concatt xss



{-5) Evalue en forma aplicativa y forma normal la siguiente expresion
and ((square 2) == 5) (inf == inf))
considerando las definiciones de cada caso:


a)
square :: Int -> Int
square x = x*x
inf :: Int -> Int
inf = inf+1
and :: Bool -> Bool -> Bool
and true true = true
and true false = false
and false true = false
and false false = false


Forma aplicativa 

and ((square 2) == 5) (inf == inf))

and ( (2*2) == 5) (inf == inf)

and (4 == 5) (inf == inf)

and (4 == 5) (inf == inf + 1)

and (4 == 5) (inf == inf + 1 + 1)



Forma Normal

=Def de and 

((square 2) == 5) (inf == inf))

{esta definicion de and obliga a saber el valor de verdad de ambas expresiones}

(2 * 2 == 5) (inf == inf)

4 == 5 (inf == inf)

False (inf == inf)

False (inf + 1 == inf )

False (inf + 1 + 1 == inf )


b) 

and ((square 2) == 5) (inf == inf))

Con forma aplicativa va a pasar exactamente lo mismo que arriba 

and ((square 2) == 5) (inf == inf))

and ( (2*2) == 5) (inf == inf)

and (4 == 5) (inf == inf)

and False (inf == inf )

and (4 == 5) (inf == inf + 1 )

and (4 == 5) (inf == inf + 1 + 1 )



Luego con forma normal


aplica def de and y luego reduce la expresion de la izquierda y obtiene el valor falso 
y luego por def de and machea con la segunda opcion y eso retorna falso directamente sin necesidad de saber el valor de verdad 
del segundo parametro 

square :: Int -> Int
square x = x*x
inf :: Int -> Int
inf = inf+1
and :: Bool -> Bool -> Bool
and true y = y
and false x = false-}



infFalse :: [Bool]
infFalse = False:infFalse