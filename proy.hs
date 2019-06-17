-- Declaraciones de tipos
-- Colores: [Rojo, Azul, Verde, Morado, Amarillo]
type Grafo = [(Int, [Int])]
type GrafoColoreado = [(Int, String)]

-- a) grafoVacio // Retorna un grafo vacio.

grafoVacio :: Grafo
grafoVacio = []


-- b) grafoNodos // Dada una lista de nodos, crea un nuevo grafo sin arcos.

grafoNodos :: [Int] -> Grafo
grafoNodos [] = []
grafoNodos (x:xs) = (x,[]) : grafoNodos(xs)


-- c) insertarNodoGrafo // Dado un grafo y nodos, retorna el grafo resultante de insertar el nodo.

insertarNodoGrafo :: Grafo -> Int -> Grafo
insertarNodoGrafo [] x = [(x,[])]
insertarNodoGrafo ys x = (x,[]):ys


-- d) insertarArcoGrafo // Dado un grafo, un nodo origen y un nodo destino, inserta un arco entre el nodo origen y el nodo destino.

insertarArcoGrafo :: Grafo -> Int -> Int -> Grafo
insertarArcoGrafo xs y z | findNode xs y && findNode xs z = [(y,z:(snd (returnNode xs y)))] ++ deleteNode xs y --                                   Se encuentran y,z en el grafo
                            | findNode xs y == True && findNode xs z == False = [(y,z:(snd(returnNode xs y)))]++[(z,[])] ++ deleteNode xs y --      Se encuentra y pero no z
                            | findNode xs y == False && findNode xs z == True = [(y,[z])]++xs --                                                    Se encuentra z pero no y
                            | not (findNode xs y && findNode xs z) && length xs /= 0 = [(y,[z]),(z,[])]++xs --                                      No se encuentran y ni z
                            | length xs == 0 = [(y,[z]),(z,[])] --                                                                                  El grafo esta vacio


-- Esta funcion elimina un Nodo del grafo

deleteNode :: Grafo -> Int -> Grafo
deleteNode [] n = [] 
deleteNode xs n = [x | x<-xs , fst x /= n]


-- Esta funcion retorna un Nodo del grafo

returnNode :: Grafo -> Int -> (Int,[Int])
returnNode (x:xs) h = if h == fst x then x else returnNode xs h


-- Esta funcion retorna True si encuentra un Nodo en el grafo, False en caso contrario

findNode :: Grafo -> Int -> Bool
findNode [] c = False
findNode (x:xs) c = if c == fst x then True else findNode xs c


-- e) culorearGrafo

colorearGrafo :: Grafo -> Int -> GrafoColoreado

colorearGrafo xs y  | validarGrafo (determinarColores (transformarGrafo xs) 1) y = pintarGrafo (determinarColores (transformarGrafo xs) 1) y
                    | otherwise = []

validarGrafo :: [(Int,[Int],Int)] -> Int -> Bool
validarGrafo [] _ = True
validarGrafo (x:xs) 5 = if (thrd x) == 31 then False else validarGrafo xs 5 
validarGrafo (x:xs) 4 = if (thrd x) >= 15 then False else validarGrafo xs 4
validarGrafo (x:xs) 3 = if (thrd x) >= 7 then False else validarGrafo xs 3
validarGrafo (x:xs) 2 = if (thrd x) >= 3 then False else validarGrafo xs 2
validarGrafo (x:xs) 1 = if (thrd x) /= 0 then False else validarGrafo xs 1

pintarGrafo :: [(Int,[Int],Int)] -> Int -> GrafoColoreado
pintarGrafo [] _ = []
pintarGrafo (x:xs) 5    | elem (thrd x) [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30] = (first x,"Rojo"):pintarGrafo xs 5
                        | elem (thrd x) [1,5,9,13,17,21,29] = (first x,"Azul"):pintarGrafo xs 5
                        | elem (thrd x) [3,11,19,27] = (first x,"Verde"):pintarGrafo xs 5
                        | elem (thrd x) [7,23,25] = (first x,"Morado"):pintarGrafo xs 5
                        | elem (thrd x) [15] = (first x,"Amarillo"):pintarGrafo xs 5

pintarGrafo (x:xs) 4    | elem (thrd x) [0,2,4,6,8,10,12,14] = (first x,"Rojo"):pintarGrafo xs 4
                        | elem (thrd x) [1,5,9,13] = (first x,"Azul"):pintarGrafo xs 4
                        | elem (thrd x) [3,11] = (first x,"Verde"):pintarGrafo xs 4
                        | elem (thrd x) [7] = (first x,"Morado"):pintarGrafo xs 4

pintarGrafo (x:xs) 3    | elem (thrd x) [0,2,4,6] = (first x,"Rojo"):pintarGrafo xs 3
                        | elem (thrd x) [1,5] = (first x,"Azul"):pintarGrafo xs 3
                        | elem (thrd x) [3] = (first x,"Verde"):pintarGrafo xs 3

pintarGrafo (x:xs) 2    | elem (thrd x) [0,2] = (first x,"Rojo"):pintarGrafo xs 2
                        | elem (thrd x) [1] = (first x,"Azul"):pintarGrafo xs 2

pintarGrafo (x:xs) 1    | elem (thrd x) [0] = (first x,"Rojo"):pintarGrafo xs 1

transformarGrafo :: Grafo -> [(Int,[Int],Int)]
transformarGrafo [] = []
transformarGrafo (x:xs) = (fst x, snd x, 0) : transformarGrafo xs

determinarColores :: [(Int,[Int],Int)] -> Int -> [(Int,[Int],Int)]
determinarColores xs n  | n <= length xs = determinarColores (asignarCont (take n xs) n  ++ drop n xs) (n+1)
                        | n > length xs = xs


asignarCont :: [(Int,[Int],Int)] -> Int -> [(Int,[Int],Int)]
asignarCont xs k = deletetripleta xs ++ [(first (last xs) , secnd (last xs) , determinarCont (xs) k)]

deletetripleta :: [(Int,[Int],Int)] -> [(Int,[Int],Int)]
deletetripleta [] = []
deletetripleta xs = take ((length xs)-1) xs

determinarCont :: [(Int,[Int],Int)] -> Int -> Int
determinarCont (x:xs) k | first x < k && elem k (secnd x) = pot2 (thrd x) + determinarCont xs k
                        | first x < k && notElem k (secnd x) = determinarCont xs k
                        | otherwise = 0

pot2 :: Int -> Int
pot2 0 = 1
pot2 1 = 2
pot2 x = 2 * (pot2 (x-1)) 

thrd :: (Int,[Int],Int) -> Int
thrd (x,y,z) = z

secnd :: (Int,[Int],Int) -> [Int]
secnd (x,y,z) = y

first :: (Int,[Int],Int) -> Int
first (x,y,z) = x


-- f) islas

islas :: Grafo -> [[Int]] --Funcion principal para resolver el problema

islas [] = []
islas xs = deleteduplicateslist (noDoble (unionlist(aux xs)))

deleteduplicateslist :: [[Int]] -> [[Int]]
deleteduplicateslist [] = []
deleteduplicateslist (x:xs) = if ismerge x xs then (deleteduplicateslist xs) else [x] ++ deleteduplicateslist xs 

aux :: Grafo -> [[Int]] --Devuelve todos los adyacentes

aux [] = [[]]
aux (x:xs) = (fst x : (ady (snd x) xs)) : aux xs

noDoble :: [[Int]] -> [[Int]] --Elimina los elementos duplicados de las listas de listas
noDoble [] = []
noDoble (x:xs) = filter (not.null) [deleteduplicates x] ++ noDoble xs

deleteduplicates :: [Int] -> [Int] -- elimina los duplicados de una lista
deleteduplicates [] = []
deleteduplicates (x:xs) = x : deleteduplicates (filter (/=x) xs)

ady :: [Int] -> Grafo -> [Int]

ady _ [] = []
ady [] _ = []
ady (z:zs) (y:ys) = if z == fst y then (z:zs) ++ (snd y) ++ ady zs ys else ady zs (y:ys)

unionlist :: [[Int]] -> [[Int]] --Une las listas
unionlist [] = [[]]
unionlist (x:xs) = if ismerge x xs then [x ++ returnlist xs] ++ unionlist ([x ++ returnlist xs] ++ droplist xs) else [justoneelement (x:xs)] ++ unionlist xs

ismerge :: [Int] -> [[Int]] -> Bool --Verifica si dos listas de pueden unir
ismerge k [] = False
ismerge [] xs = False
ismerge k (x:xs) = if foldl (||) False (map (\a -> elem (head k) x) [xs]) then True else ismerge (filter (/=head k) k) (x:xs) 


returnlist :: [[Int]] -> [Int] -- return the first node in the graph
returnlist [] = []
returnlist (x:xs) = x 


droplist :: [[Int]] -> [[Int]] -- Delete a full node in the graph
droplist [] = [[]]
droplist (x:xs) = xs

justoneelement :: [[Int]] -> [Int]
justoneelement [] = []
justoneelement (x:xs) = if (length x == 1) then x else  []