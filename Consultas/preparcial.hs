{-Ejercicios Repaso
---------------------------------------

---------------------------------------------

----------------------------------------------------   
------------------------------------------------------  

----------------------------------------------
5) Evalue en forma aplicativa y forma normal la siguiente expresion
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
b) 
square :: Int -> Int
square x = x*x
inf :: Int -> Int
inf = inf+1
and :: Bool -> Bool -> Bool
and true y = y
and false x = false-}





{-1) a) En Haskell, definir una función
 f : Int -> [Int] -> Bool  
 
que dado un número n y una lista xs, retorne true si y solo si hay repetidos en los primeros n elementos de la lista xs.
 
b) Utilice su número de documento para evaluar:
f 2 <su dni> 
con evaluación aplicativa y con evaluación normal. 
c) Qué sucede si evalúa f  3 [1..] con evaluación aplicativa, y que sucede si la evalúa usando orden normal.-}


quicksort :: (Ord a,Eq a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
              where menores = [y | y <- xs , y < x]
                    mayores = [y | y <- xs , y >= x] 


iguales :: (Eq a) => [a] -> Bool
iguales [] = True
iguales [_] = False
iguales (x:y:ys)
              | x == y = True || iguales (y:ys)
              | otherwise = iguales (y:ys)



repetidos :: (Eq a,Ord a) => Int -> [a] -> Bool
repetidos n [] = False
repetidos n xs = iguales (quicksort(take n xs))

{-

Aplicativa

repetidos 2 [4,3,3,6,7,5,7,3]
{def de repetidos}
= iguales (quicksort(take 2 [4,3,3,6,7,5,7,3]))
{def de take}
= iguales (quicksort([4,3]))
{def de quicksort}
= iguales [3,4]
{def de iguales}
=False 

Normal 

repetidos 2 [4,3,3,6,7,5,7,3]

{def de repetidos}
= iguales (quicksort(take 2 [4,3,3,6,7,5,7,3]))
{def de take }
= iguales (quicksort([4,3])
{def de quicksort}
= iguales [3,4]
{def de iguales}
=False


-}




{-2) Dada las siguientes definiciones 
K.x.y = x
inf = inf+1
a) Evaluar normal y aplicativo la siguiente expresión:
   K.3.inf 

Aplicativo :

  K.3.inf
  =def de inf 
  K.3.(inf+1)
  =def de inf
  K.3.(inf+1)+1
  =aritmetica
  K.3.inf+2
  =def de inf
  K.3.(inf+1)+2
  =aritmetica
  K.3.(inf)+3

  y nunca llegaria a la forma normal o a un resultado la funcion K



  Normal

  K.3.inf
  =def de K





-}



{-2) Dada las siguientes definiciones 
K.x.y = x
inf = inf+1
a) Evaluar normal y aplicativo la siguiente expresión:
   K.3.inf -}




{-3)
  a)Definir en haskell un tipo nuevo para árboles binarios, que distinga las hojas.
  b) Definir además una función que calcule la cantidad de hojas de un árbol.
-}


data Tree a = Nil | Node (Tree a) a (Tree a)


size :: Tree a -> Int
size Nil = 0
size (Node hi x hd) = 1 + size hi + size hd 

{-Expresiones del tipo arbol 

Nil
Node Nil 3 Nil

Node(Nil 3 Node Nil 2 Nil)



-}



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

allpares n = concat[diag n | n <-[0..n]]