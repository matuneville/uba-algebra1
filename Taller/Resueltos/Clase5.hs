
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n divisor
    | divisor == 0 = 0
    | mod n divisor == 0 = divisor + sumaDivisoresHasta n (divisor-1)
    | otherwise = sumaDivisoresHasta n (divisor-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

menorDivisor 1 = 1
menorDivisor n = menorDivisorDesde n 2
    where menorDivisorDesde :: Int -> Int -> Int
          menorDivisorDesde n divisor
            | mod n divisor == 0 = divisor
            | otherwise = menorDivisorDesde n (divisor+1)

esPrimo 1 = False
esPrimo n = menorDivisor n == n

buscarNesimoPrimo n i
    | n == 0 = i-1
    | esPrimo i = buscarNesimoPrimo (n-1) (i+1)
    | otherwise = buscarNesimoPrimo n (i+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo n = buscarNesimoPrimo n 2


esFactDesde i n
    | i > n = False
    | factorial i == n = True
    | otherwise = esFactDesde (i+1) n

    esFact :: Int -> Bool
    esFact n = esFactDesde 1 n

    menorFactDesde :: Int -> Int
    menorFactDesde n
        | esFact n = n
        | otherwise = menorFactDesde (n+1)

    mayorFactHasta :: Int -> Int
    mayorFactHasta n
        | esFact n = n
        | otherwise = mayorFactHasta (n-1)

    fibonacci :: Int -> Int
    fibonacci 0 = 0
    fibonacci 1 = 1
    fibonacci n = fibonacci (n-1) + fibonacci (n-2)

    esFibonacciDesde :: Int -> Int -> Bool
    esFibonacciDesde n i
        | n == fibonacci i = True
        | n < fibonacci i = False
        | otherwise = esFibonacciDesde n (i+1)

    esFibonacci :: Int -> Bool
    esFibonacci n = esFibonacciDesde n 0


    esSumaInicialDePrimosDesde :: Int -> Int -> Bool
    esSumaInicialDePrimosDesde n i
        | n < 0 = False
        | n == 0 = True
        | otherwise = esSumaInicialDePrimosDesde (n - nEsimoPrimo i) (i+1)

    esSumaInicialDePrimos :: Int -> Bool
    esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 1

    tomaValorMaxAux :: Int -> Int -> Int -> Int
    tomaValorMaxAux n2 m i
        | i > n2 = m
        | sumaDivisores m >= sumaDivisores i = tomaValorMaxAux n2 m (i+1)
        | sumaDivisores m < sumaDivisores i = tomaValorMaxAux n2 i (i+1)

    tomaValorMax :: Int -> Int -> Int
    tomaValorMax n1 n2 = tomaValorMaxAux n2 n1 n1

