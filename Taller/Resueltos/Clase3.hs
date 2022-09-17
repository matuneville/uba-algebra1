factorial:: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1) 
            
factorialpm 0 = 1
factorialpm n = n * factorial (n-1)

fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)
      
parteEntera:: Float -> Integer
parteEntera n | n < 1 = 0 
              | n >= 1 = 1 + parteEntera (n-1)
              
multTres n | n == 0 = True
           | n == 1 = False
           | n == 2 = False
           | n > 0 = multTres (n-3)
           
sumaImpares n | n == 0 = 0
              | n > 0 = sumaImpares (n-1)+(2*n - 1)
           
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n > 0 = n * medioFact (n-2)
            
sumDig n | n < 10 = n
         | otherwise = sumDig (div n 10) + (mod n 10)
         
digIgual n | n < 10 = True
           | otherwise = (mod n 10 == mod (div n 10) 10) && digIgual (div n 10)



            

              
