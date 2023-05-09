--    Si b2 – 4ac > 0, hay dos soluciones
--
--                                 Si b2 – 4ac = 0, hay una solución doble
--
--                                 Si b2 – 4ac < 0, no hay solución en el conjunto de los números reales

--13Defina una funci ́on que dados tres n ́umeros a, b, c devuelva la cantidad de
--ra ́ıces reales de la ecuaci ́on ax2 + bx + c = 0



raices :: Float -> Float -> Float -> [Float] 
raices a b c = [(-b -sqrt(b^2)-(4)*(a)*(c))/(2*a),(-b +sqrt(b^2)-(4)*(a)*(c))/(2*a)]