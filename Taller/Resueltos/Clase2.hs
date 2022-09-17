identidad:: a -> a
identidad x = x

primero:: a -> b -> a
primero x y = x

segundo:: a -> b -> b
segundo x y = y

constante5:: a -> b-> c -> Int
constante5 x y z = 5

triple:: (Num a) => a -> a
triple x = 3*x

f1::(Floating a, Ord a)=> a -> a-> a -> Bool
f1 x y z = x**y + z <= x + y**z

----------------------
--Ejercicios Clase 2--
----------------------

--relacionados--
estanRel::(Ord a)=> a -> a -> Bool
estanRel x y | x <= 3 && y <= 3 = True
             | x > 3 && x <= 7 && y > 3 && y <= 7 = True
             | x > 7 && y > 7 = True
             | otherwise = False

--producto interno--
prodInt:: (Int, Int) -> (Int, Int) -> Int
prodInt (vx,vy) (wx,wy) = (vx * wx) + (vy * wy) 

--todo vector menor--
todoMenor:: (Int, Int) -> (Int, Int) -> Bool
todoMenor (vx,vy) (wx,wy) | (vx < wx) && (vx < wy) = True
                          | otherwise = False

--distancia entre puntos--

distanciaPuntos (vx,vy) (wx,wy) = sqrt ((vx-wx)**2 + (vy-wy)**2)

--suma de terna--
sumaTerna:: (Num a)=> (a,a,a) -> a 
sumaTerna (x,y,z) = (x + y + z)

--posicion del primer numero par--
posicPrimerPar::(Eq a, Integral a)=> (a, a, a) -> Int
posicPrimerPar (x,y,z) | (mod x 2) == 0 = 1
                       | (mod y 2) == 0 = 2
                       | (mod z 2) == 0 = 3
                       | otherwise = 4

--crear par de numeros--
crearPar:: a -> b -> (a, b)
crearPar x y = (x, y)

--invertir par--
invertPar:: (Int, Int) -> (Int, Int)
invertPar (x, y) = (y, x)
