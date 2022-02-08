module Ambiente 
where
    import System.Random
    import System.IO.Unsafe

    data Ambiente = Ambiente
        {
            corral:: [(Int, Int)],
            ninnos:: [((Int, Int),Bool)],
            obstaculos:: [(Int, Int)],
            suciedad:: [(Int, Int)],
            robot:: [((Int, Int),Bool)]
        } deriving (Show)

--------------Aparece corral en una posicion del ambiente----------------
    hayCorral :: [((Int,Int))] -> (Int,Int) -> Bool
    hayCorral [] _ = False
    hayCorral ((i,j) : xs) (x,y) = (((i,j)==(x,y)) || hayCorral xs (x,y))  
--------------Aparece ninno en una posicion del ambiente----------------
    hayNinno :: [((Int,Int),Bool)] -> (Int,Int) -> Bool
    hayNinno [] _ = False
    hayNinno (((i,j),b) : xs) (x,y) = (((i==x) && (j==y)) || hayNinno xs (x,y))
--------------Aparecen obstaculos en una posicion del ambiente----------------
    hayObstaculos :: [(Int,Int)] -> (Int,Int) -> Bool
    hayObstaculos [] _ =False
    hayObstaculos ((i,j) : xs) (x,y) = ((((i==x) && (j==y))) || hayObstaculos xs (x,y))
--------------Aparece suciedad en una posicion del ambiente----------------
    haySuciedad :: [(Int,Int)] -> (Int,Int) -> Bool
    haySuciedad [] _ =False
    haySuciedad ((i,j) : xs) (x,y) = (((i==x) && (j==y)) || haySuciedad xs (x,y))
--------------Aparece robot en una posicion del ambiente----------------
    hayRobots :: [((Int,Int),Bool)] -> (Int,Int) -> Bool
    hayRobots [] _ =False
    hayRobots (((i,j),b) : xs) (x,y) = (((i==x) && (j==y)) || hayRobots xs (x,y))
--------------Array de 4 direcciones-----------------------
    direcciones:: [(Int,Int)]
    direcciones = [(0,1),(0,-1),(1,0),(-1,0)]
--------------Devolver adyacentes dada una posicion-----------------
    devuelveAdyacentes :: (Int,Int) -> [(Int,Int)]
    devuelveAdyacentes (x,y) = [(x+dx,y+dy) | (dx,dy) <- direcciones]
--------------Es valida dentro del tablero una posicion dada-----------------
    esValida :: Int -> Int -> (Int,Int) -> Bool
    esValida n m (x,y) = ((x >= 0) && (x < n) && (y >= 0) && (y < m))

    hay:: (Eq a)=> [a] -> a -> Bool
    hay [] _ = False
    hay (x:xs) n = ((x==n) || (hay xs n))

    elimina :: (Eq a) => [a] -> a -> [a]
    elimina [] _ = []
    elimina (x : xs) element = if x == element then xs else x : elimina xs element

