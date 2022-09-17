type Polinomio = [Float]
type Monomio = (Float, Int)

-- Punto 1 ---------------------------------------------------------------------------------------------------------------

crearPolinomio :: [Float] -> Polinomio
crearPolinomio [] = []
crearPolinomio (x:xs) | x == 0 = crearPolinomio xs
                      | otherwise = (x:xs)

-- Punto 2 ---------------------------------------------------------------------------------------------------------------

grado :: Polinomio -> Int
grado x = lengthOf x - 1

lengthOf :: Polinomio -> Int 
lengthOf [] = 0
lengthOf (x:xs) = lengthOf (xs) + 1

-- Punto 3 ---------------------------------------------------------------------------------------------------------------

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (x:xs) n = x * (n ^ grado(x:xs)) + evaluar xs n 

-- Punto 4 ---------------------------------------------------------------------------------------------------------------
productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,0) [] = []
productoPorMonomio (a,n) [] = 0 : productoPorMonomio (0,n-1) []
productoPorMonomio (a,n) (x:xs) = ((a*x) : productoPorMonomio (a,n) xs)

-- Punto 5 ---------------------------------------------------------------------------------------------------------------

producto :: Polinomio -> Polinomio -> Polinomio
producto [] (x:xs) = []
producto (y:ys) [] = []
producto [x] (y:ys) = productoPorMonomio (x,0) (y:ys)
producto (x:xs) (y:ys) = sumaProductos ((productoPorMonomio (x,grado(x:xs)) (y:ys))) (producto xs (y:ys))

sumaProductos :: Polinomio -> Polinomio -> Polinomio
sumaProductos [] [] = []
sumaProductos [] (y:ys) = (y:ys)
sumaProductos (x:xs) [] = (x:xs)
sumaProductos (x:xs) (y:ys) | grado(x:xs) == grado (y:ys) = (x+y) : sumaProductos xs ys
                            | grado(x:xs) > grado (y:ys) = x : sumaProductos xs (y:ys)
                            | otherwise = y : sumaProductos (x:xs) ys   

-- Punto 6 ---------------------------------------------------------------------------------------------------------------

evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple [] _ _ = []
evaluacionMultiple (x:xs) p q | mod (grado (x:xs)) 2 == 0 = (evaluar p x) : (evaluacionMultiple xs p q)
                              | otherwise = (evaluar q x) : (evaluacionMultiple xs p q)   

evaluacionMultiple2 :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple2 [x] p q = [evaluar p x]
evaluacionMultiple2 (x:xs) p q | grado(x:xs) `mod` 2 == 0 = (evaluar p x) : evaluacionMultiple2 xs p q
                               | otherwise = (evaluar q x) : evaluacionMultiple2 xs p q