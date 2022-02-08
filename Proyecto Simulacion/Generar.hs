module Generar
(
    generarCorral,
    generarNinnos,
    generarObstaculos,
    generarRobots,
    generarSuciedad
)
where
    import System.Random
    import Ambiente

    randomList1 :: Int -> Int-> StdGen -> [Int]
    randomList1 i f = randomRs (i, f)

    randomList2 ::Int -> Int-> StdGen -> [Int]
    randomList2 i f = randomRs (i, f)

---------------Generar Corral-----------------
    generarCorral ::(Int,Int) -> Int -> Int -> Int->[(Int,Int)] -> [(Int,Int)]
    generarCorral (x,y) n m cantC corral =
        if cantC == 1
            then 
                if (esValida n m (x,y))
                    then (x,y):corral
                    else generarCorral (0,y + 1) n m cantC corral
            else
                if (esValida n m (x,y))
                    then
                        generarCorral (x+1,y) n m (cantC-1) ((x,y):corral)
                    else
                        generarCorral (0,y+1) n m cantC corral
            

    --generaCorral:: Ambiente -> (Int,Int) -> Int -> Ambiente
    --generaCorral amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) can | can==1 =
    --    Ambiente { ninos = nin, corral= (x,y): cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)}| otherwise =
    --        if validoCorral amb (sumaTupla (x,y) (direcciones 1))
    --            then generaCorral (Ambiente { ninos = nin, corral= (x,y): cor, obstaculo = obs, robot = rob, suciedad = suc, lrandom=ran,dimension = (n,m)}) (sumaTupla (x,y) (direcciones 1)) (can-1)
    --            else generaCorral (Ambiente { ninos = nin, corral= (x,y): cor, obstaculo = obs, robot = rob, suciedad = suc, lrandom=ran,dimension = (n,m)}) (x+1,1) (can-1)


--------------- ListRandom1, ListRandom2, ninnos en ambiente, n, m, cant ninnos---------------
    generarNinnos :: [Int] -> [Int] -> [((Int,Int),Bool)] -> Int -> Int -> Int -> [((Int,Int),Bool)]  
    generarNinnos (x:xs) (y:ys) ninnos n m cant = 
        if cant == 1 
            then 
                if (esValida n m (x,y)) && not(hayNinno ninnos (x,y)) then ((x,y),True):ninnos else generarNinnos xs ys ninnos n m cant 
            else
                if (esValida n m (x,y)) && not(hayNinno ninnos (x,y)) then generarNinnos xs ys (((x,y),True):ninnos) n m (cant-1) else generarNinnos xs ys ninnos n m cant

---------------ListRandom1, ListRandom2, obstaculos en ambiente, n,m, cant obstaculos-------------
    generarObstaculos :: [Int] -> [Int] -> [(Int,Int)] -> Int -> Int -> Int -> [(Int,Int)]
    generarObstaculos (x:xs) (y:ys) obstaculos n m cant = 
        if cant == 1 
            then 
                if (esValida n m (x,y)) && not(hay obstaculos (x,y)) then (x,y):obstaculos else generarObstaculos xs ys obstaculos n m cant 
            else
                if (esValida n m (x,y)) && not(hay obstaculos (x,y)) then generarObstaculos xs ys ((x,y):obstaculos) n m (cant-1) else generarObstaculos xs ys obstaculos n m cant 

---------------ListRandom1, ListRandom2, suciedad en ambiente, n,m, cant suciedad-------------
    generarSuciedad :: [Int] -> [Int] -> [(Int,Int)] -> Int -> Int -> Int -> [(Int,Int)]
    generarSuciedad (x:xs) (y:ys) suciedad n m cant =
        if cant == 1 
            then 
                if (esValida n m (x,y)) && not(hay suciedad (x,y)) then (x,y):suciedad else generarSuciedad xs ys suciedad n m cant 
            else 
                if (esValida n m (x,y)) && not(hay suciedad (x,y)) then generarSuciedad xs ys ((x,y):suciedad) n m (cant-1) else generarSuciedad xs ys suciedad n m cant 

---------------ListRandom1, ListRandom2, robots en ambiente, n,m, cant robots-----------------
    generarRobots :: [Int] -> [Int] -> [((Int,Int),Bool)] -> Int -> Int -> Int -> [((Int,Int),Bool)]
    generarRobots (x:xs) (y:ys) robots n m cant = 
        if cant == 1 
            then 
                if (esValida n m (x,y)) && not(hayRobots robots (x,y)) then ((x,y),True):robots else generarRobots xs ys robots n m cant 
            else 
                if (esValida n m (x,y)) && not(hayRobots robots (x,y)) then generarRobots xs ys (((x,y),True):robots) n m (cant-1) else generarRobots xs ys robots n m cant


    generarAmbiente :: Int -> Int -> Int -> Int -> Int -> Int -> StdGen -> StdGen -> Ambiente
    generarAmbiente n m cant_Ch cant_O cant_D cant_R  gen1 gen2 =
        let xs = randomList1 0 (n-1) gen1
            ys = randomList2 0 (m-1) gen2
            corr = generarCorral (0,0) n m cant_Ch []
            ninn = generarNinnos xs ys [] n m cant_Ch
            robo = generarRobots xs ys [] n m cant_R
            obst = generarObstaculos xs ys [] n m cant_O
            suci = generarSuciedad xs ys [] n m cant_D
        in
            Ambiente{corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci} 
            

--cont empieza en 0
    moverNinnos :: Ambiente -> Int -> Int -> Int -> [Int] -> Ambiente
    moverNinnos ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } n m cont (xs:xxss) =
        if cont == (length ninn - 1)
            then 
                let 
                    ((x,y),b)=ninn !! cont
                    dir = xs
                in 
                    if (b == True) && (movValido ambiente  (x,y) dir) && (esValida n m ((devuelveAdyacentes (x,y)) !! dir))
                        then 
                            if hay obst ((devuelveAdyacentes (x,y)) !! dir)
                                then Ambiente {corral=corr ,ninnos = ((((devuelveAdyacentes (x,y)) !! dir),True): elimina ninn ((x,y),True)), robot = robo, obstaculos = (mueveObstaculos ambiente (x,y) dir) , suciedad = ((x,y):suci)}
                                else Ambiente {corral=corr ,ninnos = ((((devuelveAdyacentes (x,y)) !! dir),True): elimina ninn ((x,y),True)), robot = robo, obstaculos = obst, suciedad = ((x,y):suci)}
                        else 
                            ambiente
            else 
                 let 
                    ((x,y),b)=ninn !! cont
                    dir = xs 
                in 
                    if (b == True) && (movValido ambiente  (x,y) dir) && (esValida n m ((devuelveAdyacentes (x,y)) !! dir))
                        then 
                            if hay obst ((devuelveAdyacentes (x,y)) !! dir)
                                then moverNinnos (Ambiente {corral=corr ,ninnos = ((((devuelveAdyacentes (x,y)) !! dir),True): elimina ninn ((x,y),True)), robot = robo, obstaculos = (mueveObstaculos ambiente (x,y) dir) , suciedad = ((x,y):suci)}) n m (cont+1) xxss
                                else moverNinnos (Ambiente {corral=corr ,ninnos = ((((devuelveAdyacentes (x,y)) !! dir),True): elimina ninn ((x,y),True)), robot = robo, obstaculos = obst, suciedad = ((x,y):suci)}) n m (cont+1) xxss
                        else 
                            moverNinnos ambiente n m (cont + 1) xxss
            

    mueveObstaculos :: Ambiente -> (Int,Int) -> Int -> [(Int, Int)]
    mueveObstaculos ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } (x,y) dir =
        if hay obst ((devuelveAdyacentes (x,y)) !! dir) 
            then 
                (((devuelveAdyacentes (x,y)) !! dir): elimina (mueveObstaculos ambiente ((devuelveAdyacentes (x,y)) !! dir) dir) (x,y))
            else 
                (((devuelveAdyacentes (x,y)) !! dir): elimina obst (x,y))

    movValido:: Ambiente -> (Int,Int) -> Int -> Bool
    movValido ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } (x,y) dir =
        if ( (hayNinno ninn ((devuelveAdyacentes (x,y)) !! dir)) || (hayRobots robo ((devuelveAdyacentes (x,y)) !! dir)) || (hay corr ((devuelveAdyacentes (x,y)) !! dir)) || (hay suci ((devuelveAdyacentes (x,y)) !! dir)) )
            then False
            else
                if (hayObstaculos obst ((devuelveAdyacentes (x,y)) !! dir))
                    then movValido ambiente ((devuelveAdyacentes (x,y)) !! dir) dir
                    else True

    limpiar::Ambiente -> (Int, Int) -> [(Int,Int)]
    limpiar ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } pos =
        let suc = elimina suci pos
        in suc


    cargar_ninno:: Ambiente -> (Int, Int) -> Ambiente
    cargar_ninno ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } pos = 
        let robot = actualizar robo pos True
            ninnos = actualizar ninn pos True
        in Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci }

    bajar_ninno:: Ambiente -> (Int, Int) -> Ambiente
    bajar_ninno ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } pos = 
        let robot = actualizar robo pos False
            ninnos = actualizar ninn pos False
        in Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci }

    actualizar:: [((Int,Int), Bool)] -> (Int,Int) -> Bool -> [((Int,Int), Bool)]
    actualizar [] _ _ = [] 
    actualizar ((x,k):xs) pos b = if x == pos then (x,b) : xs else ((x,k) : (actualizar xs pos b))

    bfs:: Ambiente -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> Int -> Int -> Int
    bfs amb@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } (x,y) buscados visitados n m =
        if (hay buscados (x,y)) then 0 else 
            if ((hayNinno ninn (x,y)) || (hayRobots robo (x,y)) || (hay obst (x,y))) || not(esValida n m (x,y)) || (hay visitados (x,y)) then -1 else
                minimo ((bfs amb (x-1,y) buscados ((x,y):visitados) n m):(bfs amb (x+1,y) buscados ((x,y):visitados) n m):(bfs amb (x,y-1) buscados ((x,y):visitados) n m):(bfs amb (x,y+1) buscados ((x,y):visitados) n m):[])

    minimo:: [Int] -> Int
    minimo [] = -1
    minimo (x:xs) = if x == -1 then minimo xs else minimo2 x (minimo xs)

    minimo2:: Int -> Int -> Int
    minimo2 a b = if a == -1 then b else if b == -1 then a else min a b

    minPos:: [((Int,Int),Int)] -> ((Int,Int),Int)
    minPos [] = ((-1,-1),-1)
    minPos (((x,y),v):xs) = if v == -1 then minPos xs else minPos2 ((x,y),v) (minPos xs)

    minPos2:: ((Int,Int),Int) -> ((Int,Int),Int) -> ((Int,Int),Int)
    minPos2 (x,y) (a,b) = if y == -1 then (a,b) else
         if b == -1 then (x,y) else 
             if y > b then (a,b) else (x,y)

    quitarBool:: [((Int,Int),Bool)] -> [(Int,Int)]
    quitarBool []=[]
    quitarBool ((x,b):xs) = x: quitarBool xs

--cont comienza en 0
    corralDisponible:: Ambiente -> Int -> (Int,Int)
    corralDisponible amb@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } cont =
        if cont == ((length corr) - 1)
            then 
                let
                    (x,y)=corr !! (((length corr)-1)-cont)
                in
                    if (hayNinno ninn (x,y)) || (hayRobots robo (x,y))
                        then (-1,-1)
                        else (x,y)
            else
                let
                    (x,y)=corr !! (((length corr)-1)-cont)
                in
                    if (hayNinno ninn (x,y)) || (hayRobots robo (x,y))
                        then corralDisponible amb (cont+1)
                        else (x,y)

    moverRobots1:: Ambiente -> Int -> Int -> Int -> Ambiente
    moverRobots1 ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } cont n m =
        if cont == ((length robo) - 1)
            then
                let
                    ((x,y),b)= robo !! cont
                in
                    if b
                        then
                            let
                                ((xn,yn),cn) = minPos( ((x-1,y),(bfs ambiente (x-1,y) (quitarBool ninn) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) (quitarBool ninn) [] n m)):((x,y-1),(bfs ambiente (x,y-1) (quitarBool ninn) [] n m)):((x,y+1),(bfs ambiente (x,y+1) (quitarBool ninn) [] n m)):[] ) 

                                ((xs,ys),cs)= minPos( ((x-1,y),(bfs ambiente (x-1,y) suci [] n m) ): ((x+1,y),(bfs ambiente (x+1,y) suci [] n m)):((x,y-1),(bfs ambiente (x,y-1) suci [] n m)):((x,y+1),(bfs ambiente (x,y+1) suci [] n m)):[] )
                            in
                                if (x,y)==(xn,yn)
                                    then
                                        cargar_ninno ambiente (x,y)
                                    else
                                        if cn == -1
                                            then
                                                if cs == -1
                                                    then ambiente
                                                    else
                                                        Ambiente {corral=corr ,ninnos = ninn, robot = (((xs,ys),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = elimina suci (xs,ys) }
                                            else
                                                Ambiente {corral=corr ,ninnos = ninn , robot = (((xn,yn),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad =elimina suci (xn,yn) }
                        else
                            let
                                (cdx,cdy)=(corralDisponible ambiente 0)
                            in
                                if cdx == -1
                                    then ambiente
                                    else
                                        let
                                            ((xc,yc),cc)=minPos( ((x-1,y),(bfs ambiente (x-1,y) ((corralDisponible ambiente 0):[]) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) ((corralDisponible ambiente 0):[]) [] n m)):((x,y-1),(bfs ambiente (x,y-1) ((corralDisponible ambiente 0):[]) [] n m)):((x,y+1),(bfs ambiente (x,y+1) ((corralDisponible ambiente 0):[]) [] n m)):[] )
                                        in
                                            if (x,y)==(xc,yc)
                                                then
                                                    bajar_ninno ambiente (x,y)
                                                else
                                                    Ambiente {corral=corr ,ninnos = ninn, robot = (((xc,yc),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = suci }
            else
                let
                    ((x,y),b)= robo !! cont
                in
                    if b
                        then
                            let
                                ((xn,yn),cn) = minPos( ((x-1,y),(bfs ambiente (x-1,y) (quitarBool ninn) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) (quitarBool ninn) [] n m)):((x,y-1),(bfs ambiente (x,y-1) (quitarBool ninn) [] n m)):((x,y+1),(bfs ambiente (x,y+1) (quitarBool ninn) [] n m)):[] ) 

                                ((xs,ys),cs)=minPos( ((x-1,y),(bfs ambiente (x-1,y) suci [] n m) ):((x+1,y),(bfs ambiente (x+1,y) suci [] n m)):((x,y-1),(bfs ambiente (x,y-1) suci [] n m)):((x,y+1),(bfs ambiente (x,y+1) suci [] n m)):[] ) 
                            in
                                if (x,y)==(xn,yn)
                                    then
                                        moverRobots1 (cargar_ninno ambiente (x,y)) (cont+1) n m
                                    else
                                        if cn == -1
                                            then
                                                if cs == -1
                                                    then moverRobots1 ambiente (cont+1) n m
                                                    else
                                                        moverRobots1 (Ambiente {corral=corr ,ninnos = ninn, robot = (((xs,ys),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad =elimina suci (xs,ys) }) (cont+1) n m
                                            else
                                                moverRobots1 (Ambiente {corral=corr ,ninnos = ninn , robot = (((xn,yn),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = elimina suci (xn,yn) }) (cont+1) n m
                        else
                            let
                                (cdx,cdy)=(corralDisponible ambiente 0)
                            in
                                if cdx == -1
                                    then ambiente
                                    else
                                        let
                                            ((xc,yc),cc)=minPos( ((x-1,y),(bfs ambiente (x-1,y) ((corralDisponible ambiente 0):[]) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) ((corralDisponible ambiente 0):[]) [] n m)):((x,y-1),(bfs ambiente (x,y-1) ((corralDisponible ambiente 0):[]) [] n m)):((x,y+1),(bfs ambiente (x,y+1) ((corralDisponible ambiente 0):[]) [] n m)):[] )
                                        in
                                            if (x,y)==(xc,yc)
                                                then
                                                    moverRobots1 (bajar_ninno ambiente (x,y)) (cont+1) n m
                                                else
                                                    moverRobots1 (Ambiente {corral=corr ,ninnos = ninn, robot = (((xc,yc),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = suci  }) (cont+1) n m

    moverRobots2:: Ambiente -> Int -> Int -> Int -> Ambiente
    moverRobots2 ambiente@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } cont n m =
        if cont == ((length robo) - 1)
            then
                let
                    ((x,y),b)= robo !! cont
                in
                    if b
                        then
                            let
                                ((xn,yn),cn) = minPos( ((x-1,y),(bfs ambiente (x-1,y) (quitarBool ninn) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) (quitarBool ninn) [] n m)):((x,y-1),(bfs ambiente (x,y-1) (quitarBool ninn) [] n m)):((x,y+1),(bfs ambiente (x,y+1) (quitarBool ninn) [] n m)):[] ) 

                                ((xs,ys),cs)= minPos( ((x-1,y),(bfs ambiente (x-1,y) suci [] n m) ): ((x+1,y),(bfs ambiente (x+1,y) suci [] n m)):((x,y-1),(bfs ambiente (x,y-1) suci [] n m)):((x,y+1),(bfs ambiente (x,y+1) suci [] n m)):[] )
                            in
                                if (x,y)==(xn,yn)
                                    then
                                        cargar_ninno ambiente (x,y)
                                    else
                                        if cs == -1
                                            then
                                                if cn == -1
                                                    then ambiente
                                                    else
                                                        Ambiente {corral=corr ,ninnos = ninn, robot = (((xn,yn),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = elimina suci (xn,yn) }
                                            else
                                                Ambiente {corral=corr ,ninnos = ninn , robot = (((xs,ys),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = elimina suci (xs,ys) }
                        else
                            let
                                (cdx,cdy)=(corralDisponible ambiente 0)
                            in
                                if cdx == -1
                                    then ambiente
                                    else
                                        let
                                            ((xc,yc),cc)=minPos( ((x-1,y),(bfs ambiente (x-1,y) ((corralDisponible ambiente 0):[]) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) ((corralDisponible ambiente 0):[]) [] n m)):((x,y-1),(bfs ambiente (x,y-1) ((corralDisponible ambiente 0):[]) [] n m)):((x,y+1),(bfs ambiente (x,y+1) ((corralDisponible ambiente 0):[]) [] n m)):[] )
                                        in
                                            if (x,y)==(xc,yc)
                                                then
                                                    bajar_ninno ambiente (x,y)
                                                else
                                                    Ambiente {corral=corr ,ninnos = ninn, robot = (((xc,yc),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = suci }
            else
                let
                    ((x,y),b)= robo !! cont
                in
                    if b
                        then
                            let
                                ((xn,yn),cn) = minPos( ((x-1,y),(bfs ambiente (x-1,y) (quitarBool ninn) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) (quitarBool ninn) [] n m)):((x,y-1),(bfs ambiente (x,y-1) (quitarBool ninn) [] n m)):((x,y+1),(bfs ambiente (x,y+1) (quitarBool ninn) [] n m)):[] ) 

                                ((xs,ys),cs)=minPos( ((x-1,y),(bfs ambiente (x-1,y) suci [] n m) ):((x+1,y),(bfs ambiente (x+1,y) suci [] n m)):((x,y-1),(bfs ambiente (x,y-1) suci [] n m)):((x,y+1),(bfs ambiente (x,y+1) suci [] n m)):[] ) 
                            in
                                if (x,y)==(xn,yn)
                                    then
                                        moverRobots2 (cargar_ninno ambiente (x,y)) (cont+1) n m
                                    else
                                        if cs == -1
                                            then
                                                if cn == -1
                                                    then moverRobots2 ambiente (cont+1) n m
                                                    else
                                                        moverRobots2 (Ambiente {corral=corr ,ninnos = ninn, robot = (((xn,yn),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = elimina suci (xn,yn) }) (cont+1) n m
                                            else
                                                moverRobots2 (Ambiente {corral=corr ,ninnos = ninn , robot = (((xs,ys),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = elimina suci (xs,ys) }) (cont+1) n m
                        else
                            let
                                (cdx,cdy)=(corralDisponible ambiente 0)
                            in
                                if cdx == -1
                                    then ambiente
                                    else
                                        let
                                            ((xc,yc),cc)=minPos( ((x-1,y),(bfs ambiente (x-1,y) ((corralDisponible ambiente 0):[]) [] n m) ):((x+1,y),(bfs ambiente (x+1,y) ((corralDisponible ambiente 0):[]) [] n m)):((x,y-1),(bfs ambiente (x,y-1) ((corralDisponible ambiente 0):[]) [] n m)):((x,y+1),(bfs ambiente (x,y+1) ((corralDisponible ambiente 0):[]) [] n m)):[] )
                                        in
                                            if (x,y)==(xc,yc)
                                                then
                                                    moverRobots2 (bajar_ninno ambiente (x,y)) (cont+1) n m
                                                else
                                                    moverRobots2 (Ambiente {corral=corr ,ninnos = ninn, robot = (((xc,yc),True):(elimina robo ((x,y),True))), obstaculos = obst, suciedad = suci }) (cont+1) n m


--Crear====================================================
    main:: Int -> Int -> Int-> Int -> Int -> Int -> Int -> IO()
    main n m cantNinnos cantRobots cantObstaculos cantSuciedad time=
        let
            (_,gen1)= random (mkStdGen 100 )::(Int,StdGen)
            (_,gen2)= random (mkStdGen 150 )::(Int,StdGen)
            amb = generarAmbiente n m cantNinnos cantObstaculos cantSuciedad cantRobots gen1 gen2
        in
            mover amb amb time time n m

    mover:: Ambiente -> Ambiente -> Int -> Int -> Int -> Int -> IO()
    mover ambiente1@Ambiente {corral=corr ,ninnos = ninn, robot = robo, obstaculos = obst, suciedad = suci } ambiente2@Ambiente {corral=corr2 ,ninnos = ninn2, robot = robo2, obstaculos = obst2, suciedad = suci2 } time cont n m =
        let
            (_,gen)= random (mkStdGen (35*(length suci)) )::(Int,StdGen)
        in
            if cont == 0
                then
                    let
                        (_,gen1)= random (mkStdGen (35*(length suci)) )::(Int,StdGen)
                        (_,gen2)= random (mkStdGen (27*(length suci)) )::(Int,StdGen)

                    in
                        --mover (generarAmbiente n m (length ninn) (length obst) (length suci) (length robo) gen1 gen2) time time n m
                        do
                            print ambiente1
                            print ambiente2
                else do
                    mover (moverNinnos (moverRobots1 ambiente1 0 n m) n m 0 (randomList1 0 3 gen)) (moverNinnos (moverRobots2 ambiente2 0 n m) n m 0 (randomList1 0 3 gen)) time (cont-1) n m