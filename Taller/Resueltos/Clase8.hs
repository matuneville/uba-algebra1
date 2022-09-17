fact:: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)
------------------------------------------
comb:: Int -> Int -> Int 
comb n k = div (fact n) ((fact (n-k)) * (fact k))

comb2:: Int -> Int -> Int 
comb2 n k = (comb (n-1) k) + (comb (n-1) (k-1))

------------------------------------------
type Set a = [a]

vacio:: Set a 
vacio = []

agregar:: Eq a => a -> Set a -> Set a
agregar n k | elem n k = k
            | otherwise = n:k

union:: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

------------------------------------------
variaciones:: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElemAListas c (variaciones c (k-1))

agregarElemAListas:: Set Int -> Set [Int] -> Set [Int]
agregarElemAListas [] _ = []
agregarElemAListas (x:xs) c = union (agregarElemAdelante x c) (agregarElemAListas (xs) c)

agregarElemAdelante:: Int -> Set [Int] -> Set [Int] 
agregarElemAdelante _ [] = []
agregarElemAdelante x (ys:yss) = agregar (x:ys) (agregarElemAdelante x yss) 