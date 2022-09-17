--1--
f1:: Int -> Int
f1 n | n == 0 = 1
     | otherwise = 2 ^ n + f1 (n - 1)

--2--
f2:: Int -> Float -> Float
f2 n q | n == 0 = 0
       | otherwise = q ^ n + f2 (n - 1) q

--3--
f3:: Int -> Float -> Float
f3 n q | n == 0 = 0
       | otherwise = q ^ (2*n-1) + q ^ (2*n) + (f3 (n-1) q)

--4--
f4:: Int -> Float -> Float
f4 n q | n == 0 = 1
       | otherwise = q ^ (2*n -1) + q ^ (2*n) - q ^ (n-1) + (f4 (n-1) q)

--Factorial--
fact:: Int -> Int
fact n | n == 0 = 1
       | otherwise = n * (fact (n-1))

eAprox:: Int -> Float
eAprox n | n == 0 = 1
         | otherwise = 1 / (fromIntegral (fact n)) + eAprox (n-1)

e::Float
e = eAprox 10

--doble sumatoria--
f n m | n == 0 = 0
      | otherwise = round (f2 m (fromIntegral n)) + (f(n-1) m)

-----EJERCICIOS-----

g1 i 0 = 0
g1 i n = (g1 i (n-1)) + i ^ n

g2 1 n = 1*n 
g2 i n = g1 i n + g2 (i-1) n 

g20 n = g2 n n