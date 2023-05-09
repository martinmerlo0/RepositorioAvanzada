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