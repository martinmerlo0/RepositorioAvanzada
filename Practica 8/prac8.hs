{-.


4. Utilizando las ideas asociadas a listas por comprensi ́on, y las funciones
sum, product, y length, escribir los cuantificadores de sumatoria, productoria y
contatoria para ejemplos concretos.-}


--1. Definir la funci ́on nand a b = not (a && b) en Haskell sin utilizar not y &&

nand :: Bool -> Bool -> Bool
nand True True = False 
nand _ _ = True 


{-2. Definir en Haskell la funci ́on
maj : : Bool −> Bool −> Bool −> Bool
−−retorna True sii al menos 2 argumentos son True-}


maj :: Bool -> Bool -> Bool -> Bool
maj True True True = True 
maj True True False = True
maj True False True = True 
maj False True True = True 
maj _ _ _ = False 


{-3. En Haskell un predicado sobre un tipo A es una funcion p :: A −> Bool,
por ejemplo:
even : : I n t −> Bool
even x = x ‘mod‘ 2 == 0
Se puede pensar como un predicado sobre n ́umeros cuya variable libre es x.
Adem ́as en Haskell tenemos las siguientes funciones que operan sobre listas de
booleanos:
and : : [ Bool ] −> Bool
−− r e t o r n a True s i i t od o s l o s el em e n t o s son True
o r : : [ Bool ] −> Bool
−− r e t o r n a True s i i a l menos un elemen t o e s True
Con estos dos operadores y listas por comprensi ́on podemos escribir una
versi ́on ejecutable de los cuantificadores en Haskell. Por ejemplo el siguiente
cuantificador:

(∀i : 0 ≤ i < ]xs : even xs.i)

Puede escribirse literalmente como:

and [ even xs ! ! i | i <− [ 0 . . ( l e n g t h xs ) −1]]
Pero en Haskell lo m ́as com ́un es escribirlo de la siguiente forma:
and [ even x | x <− xs ]
Utilizar estas ideas para escribir los siguientes cuantificadores:
• (∃i : 0 ≤ i < ]xs : p xs.i)
• (∀i : 0 ≤ i < ]xs : p xs.i)
Para un predicado p dado.-}