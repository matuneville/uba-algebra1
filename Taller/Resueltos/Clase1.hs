----EJERCICIOS CLASE 1----
--absolutos--
absoluto :: Float -> Float
absoluto a | a >= 0 = a
           | otherwise = a * (-1)

maximoabsoluto :: Float -> Float -> Float
maximoabsoluto a b | (absoluto a) >= (absoluto b) = (absoluto a)
                   | otherwise = (absoluto b)

maximo3 :: Float -> Float -> Float -> Float
maximo3 a b c | (absoluto a) >= (absoluto b) && (absoluto a) >= (absoluto c) = (absoluto a)
              | (absoluto b) >= (absoluto a) && (absoluto b) >= (absoluto c) = (absoluto b)
              | otherwise = (absoluto c)

--alguno es 0--
algunoEs0 :: Float -> Float -> Bool 
algunoEs0 a b | a == 0 || b == 0 = True
              | otherwise = False

ambosSon0 :: Float -> Float -> Bool 
ambosSon0 a b | a == 0 && b == 0 = True
              | otherwise = False

--los mismos usando pattern matching--
algunoEs0pm :: Float -> Float -> Bool 
algunoEs0pm 0 b = True
algunoEs0pm a 0 = True
algunoEs0pm a b = False

ambosSon0pm :: Float -> Float -> Bool 
ambosSon0pm 0 0 = True
ambosSon0pm a b = False 

--multiplos--
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b | (a `mod` b) == 0 = True
                 | otherwise = False