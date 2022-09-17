type Set a = [a]

vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (c:cs) = x == c || pertenece x cs

agregar :: Int -> Set Int -> Set Int
agregar x ys
    | pertenece x ys = ys
    | otherwise = x : ys

incluido :: Set Int -> Set Int -> Bool
incluido [] ys = True
incluido (x:xs) ys = pertenece x ys && incluido xs ys

iguales xs ys = incluido xs ys && incluido ys xs

union :: Set Int -> Set Int -> Set Int
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC _ [] = False
perteneceC x (y:ys) = iguales x y || perteneceC x ys

agregarC x ys
    | perteneceC x ys = ys
    | otherwise = x : ys

unionC [] ys = ys
unionC (x:xs) ys = unionC xs (agregarC x ys)

agregarATodos _ [] = []
agregarATodos x (y:ys) = agregarC (agregar x y) (agregarATodos x ys)

productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = armarTuplas x ys ++ productoCartesiano xs ys

armarTuplas :: Int -> Set Int -> Set (Int, Int)
armarTuplas x [] = []
armarTuplas x (y:ys) = [(x,y)] ++ armarTuplas x ys
