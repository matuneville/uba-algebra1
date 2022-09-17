--------------------------   1    -------------------------------------

satisfaceCollatz :: Integer -> Integer -> Bool
satisfaceCollatz 1 m = (m>0)
satisfaceCollatz n m | mod n 2 == 0 = satisfaceCollatz (div n 2) (m-1)
                     | mod n 2 /= 0 = satisfaceCollatz (3*n+1) (m-1)
                     | otherwise= False



--1) satisfaceCollatz y satisfaceCollatzAux: --Si es par, hacer m/2. si es impar, hacer (3*m)+1, caso base m=1, necesita cero pasos para llegar al 1
---- Si no pongo el +1 siempre me va a dar cero. Por cada paso que se cumple me suma 1
--El +1 sirve para contar los pasos. Ejemplo: si es 1 la hace en 0 pasos. 
--Si el numero es 2, ingresa por el lado de pares, hace satisfaceCollatzAux(div 2 2),
--osea satisfaceCollatz (1) le suma 1, y como satisfaceCollatz de 1 es el caso base y da cero, entonces todo da 1,
-- y esta es la cantidad de pasos

--------------------------   2    -------------------------------------

satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta n  m | n == 1 && m > 0 = True
                           | satisfaceCollatz n m = satisfaceCollatzHasta (n-1) m
                           | otherwise = False


--2) satisfaceCollatzHasta: -- si satisfaceCollatz n m  es true, hace satisfaceCollatzHasta (n-1) m. si es false no.
-- 2 1

--------------------------   3    -------------------------------------

cantidadTerminosPares :: Integer -> Integer 
cantidadTerminosPares n | n == 1 = 0
                        | mod n 2 == 0 = cantidadTerminosPares(div n 2) + 1
                        | mod n 2 == 1 = cantidadTerminosPares(3*n + 1)

--3) cantidadTerminosPares : - si es par entra por el segundo renglon,(==0) y suma 1, si es impar entra por el 3er renglón (==1) y no suma nada,
-- así hasta llegar a n=1

-------------------------    4    -------------------------------------

largoSecuencia :: Integer->Integer
largoSecuencia n | n==1 = 0
                 | mod n 2 == 0 = largoSecuencia (div n 2) + 1
                 | mod n 2 == 1 = largoSecuencia (3*n + 1) + 1

--4) igual que auxiliar de la 1

-------------------------    5    -------------------------------------

secuenciaMasLargaHasta :: Integer -> Integer 
secuenciaMasLargaHasta 1 = 1
secuenciaMasLargaHasta n = masLarga n (n-1)

masLarga :: Integer -> Integer -> Integer 
masLarga n k | k == 1 = n 
             | largoSecuencia n > largoSecuencia k = masLarga n (k-1) 
             | largoSecuencia n <= largoSecuencia k  = masLargaAux (k-1) k 


masLargaAux :: Integer -> Integer -> Integer 
masLargaAux n k | k == 1 = n 
                | largoSecuencia n > largoSecuencia k = masLargaAux n (k-1)
                | largoSecuencia n >= largoSecuencia k = masLargaAux k (k-1)
                | otherwise = masLarga k (n-1)

--5) secuenciaMasLargaHasta: -- dado n, devuelve m que genera la secuencia más larga de pasos hasta n.
-- Ejemplo 16: hace masLarga 16 15, -> masLarga entra por la 3er guarda,
-- y hace masLargaAux 14 15, ahí 14 y 15 tienen el mismo largo, entran por la segunda
-- ya que son iguales y hace masLargaAux 14 14, son iguales, hace masLargaAux 14 13
-- hasta masLargaAux 14 9 (largosecuencia 9: 19), ahí entra por la 3er guarda
-- y hace masLarga 9 8 , entra por la segunda guarda, y hace masLarga 9 7
-- vuelve por la misma, y así hasta 9 1, donde detecta que k=1, y devuelve n
-- osea 9. Entonces la secuenciaMasLarga hasta 16 es 9