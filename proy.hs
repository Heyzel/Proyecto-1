-- Declaraciones de tipos

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

-- Esta funcion retorna la lista de un Nodo en un Grafo

returnList :: Grafo -> Int -> [Int] 
returnList (x:xs) k = k : snd x