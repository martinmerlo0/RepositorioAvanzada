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


-- ---------------------------------------------------- 
-- maj retorna True sii al menos 2 argumentos son True.
-- ----------------------------------------------------


-- ---------------------------------------------------- 
-- Para las siguientes funciones se debe respetar el 
-- perfil propuesto.
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a].
-- Mientras que (Int -> [a] -> Bool) es la propiedad.
--		Ejemplo: paraTodo [0,1,2,3] [4,1,2,6] even 
--		retorna False, ya que existe una posición 
--		en la que el elemento de la lista es impar. 
--		paraTodo [0,2,4,6] [2,2,4,4,4,5,6] even  
--		retorna True.
-- ----------------------------------------------------
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys f = and [f i ys | i <-xs]



even' :: Int -> [Int] -> Bool
even' n xs = even (xs !! n) -- veo si el elemento en la posicion n es par 


-- ----------------------------------------------------
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a]. 
-- (Int -> [a] -> Bool) es la propiedad.
--
--		Ejemplo: existe [0,1,2,3] [4,1,2,6] odd
--		retorna True.
-- ----------------------------------------------------
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys f = or [f i ys | i <- xs]




sumatoria :: (Num a) => [a] -> a
sumatoria xs = sum [x | x <- xs]


productoria :: (Num a) => [a] -> a 
productoria [] = 1
productoria xs = product [x | x <- xs]


contatoria :: [a] -> Int
contatoria xs = sum [1 | x <- xs , f x]