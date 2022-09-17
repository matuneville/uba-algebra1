primerosNpares:: Int -> [Int]
primerosNpares n | n == 0 = []
                 | otherwise = (2*(n-1)) : primerosNpares (n-1)

sumatoriaPM [] = 0
sumatoriaPM (n:m) = n +sumatoriaPM m

productoriaPM [] = 1
productoriaPM (x:xs) = x + productoriaPM xs

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (n + x) : sumarN n xs

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = sumarN x (x:xs)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo (x:xs) = sumarN (ultimo (x:xs)) (x:xs)

ultimo [x] = x
ultimo (x:xs) = ultimo xs

pares:: [Int] -> [Int]
pares [] = []
pares (x:xs) | (mod x 2) == 0 = x : pares xs
             | otherwise = pares xs

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | (mod x n) == 0 = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

quitar :: Int-> [Int] -> [Int]
quitar a [] = []
quitar a (x:xs) | x == a = xs
                | otherwise = x : (quitar a xs)

hayRepetidos:: [Int] -> Bool
hayRepetidos (x:xs) | (x:xs) == [] = False
                    | x == xs = True
                    | x /= xs = hayRepetidos (x:xs)
