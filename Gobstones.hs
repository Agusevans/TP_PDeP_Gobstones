{----------------------------------------------------------------------------------
    Trabajo Práctico Gobstones 
    Grupo 12
 ----------------------------------------------------------------------------------}

-- Imports 

import Data.List
import Text.Show.Functions

--Modelos de Estructuras --
-- 1.

data Tablero = Tablero { 
    filas :: Int ,
    columnas :: Int,
    celdas :: [Celda],
    cabezal :: Coordenadas
} 

type Coordenadas = (Int, Int)

data Celda = Celda {
    posicion :: Coordenadas,
    bolitas :: [Bolita]
} deriving (Show, Eq)

data Bolita = Verde | Negro | Azul | Rojo deriving (Show, Eq)

data Direccion = Norte | Sur | Este | Oeste deriving (Show, Eq)

type Sentencia = (Tablero -> Tablero)

type Condicion = (Tablero -> Bool)

-- Inicializar Tablero --
-- 2.

inicializarTablero :: Int -> Int -> Tablero
inicializarTablero filas columnas 
    | esTableroValido filas columnas = Tablero filas columnas (crearListaCeldas filas columnas) (1,1)
    | otherwise = error "No es un tablero valido"

esTableroValido filas columnas = filas > 0 && columnas >0

crearListaCeldas :: Int -> Int -> [Celda]
crearListaCeldas filas columnas = map (\coordenadas -> Celda coordenadas []) (prodCartesiano [1..columnas] [1..filas])

prodCartesiano listaColumnas listaFilas = 
    concatMap (\columna -> map (\fila -> (columna,fila))  listaFilas ) listaColumnas 

-------------------
tableroInicial :: Tablero
tableroInicial = inicializarTablero 3 3
-------------------

-- Sentencias Primitivas --

-- 3.a.
moverCoordenadas :: Coordenadas -> Direccion -> Coordenadas
moverCoordenadas (columna, fila) direccion 
    | direccion == Norte = (columna, fila+1)
    | direccion == Sur = (columna, fila-1)
    | direccion == Este = (columna+1, fila)
    | direccion == Oeste = (columna-1, fila)

moverCabezal :: Direccion -> Tablero -> Tablero
moverCabezal direccion (Tablero filas columnas celdas cabezal)
    | puedeMoverse direccion (Tablero filas columnas celdas cabezal) = Tablero filas columnas celdas (moverCoordenadas cabezal direccion)
    | otherwise   = error "Se cayó del tablero"

-- 3.b. 
hacerSiCoincide cabezal bolita funcion celda 
    | cabezal == posicion celda = funcion bolita celda
    | otherwise = celda

agregarBolita bolita celda = celda { bolitas = bolita : bolitas celda }

buscarYAgregar :: [Celda] -> Coordenadas -> Bolita -> [Celda]
buscarYAgregar celdas cabezal bolita = map (hacerSiCoincide cabezal bolita agregarBolita) celdas

ponerBolita :: Bolita -> Tablero -> Tablero
ponerBolita  bolita (Tablero filas columnas celdas cabezal) = 
    Tablero filas columnas (buscarYAgregar celdas cabezal bolita) cabezal

-- 3.c. 

hayBolitaDeColorEnCelda bolita celda = elem bolita (bolitas celda)

sacarSiHayBolita bolita celda 
    | hayBolitaDeColorEnCelda bolita celda = celda {bolitas = (delete bolita (bolitas celda))}
    | otherwise = error ("No se puede sacar bolita porque no hay ninguna de color " ++ (show bolita))

buscarYSacar :: [Celda] -> Coordenadas -> Bolita -> [Celda]
buscarYSacar celdas cabezal bolita = map (hacerSiCoincide cabezal bolita sacarSiHayBolita) celdas

sacarBolita :: Bolita -> Tablero -> Tablero
sacarBolita bolita (Tablero filas columnas celdas cabezal)  = 
    Tablero filas columnas (buscarYSacar celdas cabezal bolita) cabezal

-- Sentencias Compuestas --

-- 4.a.

composicionListaDeFunciones funciones = foldl (.) id (reverse funciones)

repetirListaDeFunciones funciones repeticiones = take (repeticiones * length funciones) (cycle funciones)

repetir funciones repeticiones = 
    composicionListaDeFunciones . repetirListaDeFunciones funciones $ repeticiones

-- 4.b.

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion sentenciasTrue sentenciasFalse tablero
    | condicion tablero = (composicionListaDeFunciones sentenciasTrue) tablero
    | otherwise = (composicionListaDeFunciones sentenciasFalse) tablero
    
si :: Condicion -> [Sentencia] -> Tablero -> Tablero
si condicion sentencias tablero = alternativa condicion sentencias [] tablero

sino :: Condicion -> [Sentencia] -> Tablero -> Tablero
sino condicion sentencias tablero = alternativa condicion [] sentencias tablero

-- 4.c.  (Faltaria no repetir logica, pero asi anda)

mientras :: Condicion -> [Sentencia] -> Tablero -> Tablero
mientras condicion sentencias tablero
    | condicion tablero = mientras condicion sentencias ((composicionListaDeFunciones sentencias) tablero) 
    | otherwise = tablero

-- 4.d- 

irAlBorde direccion = mientras (puedeMoverse direccion) [moverCabezal direccion] 

-- Condiciones --

-- 5.a. 

puedeMoverse direccion tablero = 
    estaEnTablero tablero {cabezal = moverCoordenadas (cabezal tablero) direccion} 

estaEnTablero (Tablero filas columnas celdas (columna, fila)) = 
    columna <= columnas && columna > 0 && fila<=filas && fila > 0

-- 5.b. 

celdaActual (Tablero filas columnas celdas cabezal) =
    head . filter (\celda -> posicion celda == cabezal) $ celdas

hayBolita bolita tablero = hayBolitaDeColorEnCelda bolita (celdaActual tablero)

-- 5.c.

cantidadBolitasEnCelda bolita celda = length . filter ( == bolita) $ (bolitas celda)
cantidadBolitas bolita tablero = cantidadBolitasEnCelda bolita (celdaActual tablero)

-- Instruccion programa --
-- 6.

programa tablero sentencias = composicionListaDeFunciones sentencias $ tablero

-- Programa ejemplo
-- 7.

program = [
    moverCabezal Norte,
    ponerBolita Negro,
    ponerBolita Negro,
    ponerBolita Azul,
    moverCabezal Norte,
    repetir [
        ponerBolita Rojo,
        ponerBolita Azul
        ] 15 ,
    alternativa (hayBolita Verde) [
        moverCabezal Este, ponerBolita Negro
        ][
             moverCabezal Sur, moverCabezal Este, ponerBolita Azul],
    moverCabezal Este,
    mientras ( (<= 9).(cantidadBolitas Verde) ) [
        ponerBolita Verde] ,
    ponerBolita Azul
    ]

tableroFinal = programa tableroInicial  program

-- Display tablero --

indicarCeldaActual displayCelda = "<<" ++ displayCelda ++ ">>   |   "

displayBolitasCelda tablero celda 
    | celda == (celdaActual tablero) = indicarCeldaActual (displayBolitasNormal celda)
    | otherwise = displayBolitasNormal celda ++ "   |   "

displayBolitasNormal celda = 
    show (posicion celda) ++ "=[ " ++
    displayBolitasDeColor (cantidadBolitasEnCelda Azul  celda) "A" ++ 
    displayBolitasDeColor (cantidadBolitasEnCelda Negro celda) "N" ++  
    displayBolitasDeColor (cantidadBolitasEnCelda Rojo  celda) "R" ++ 
    displayBolitasDeColor (cantidadBolitasEnCelda Verde celda) "V" ++ " ]"

displayBolitasDeColor 0 _ = ""
displayBolitasDeColor cantidad color =  "("++ show cantidad ++ color ++ ")"

displayFila tablero listaDeCeldas  = concatMap (displayBolitasCelda tablero ) listaDeCeldas

--------------

celdasEnFila tablero fila = filter ((==fila).snd.posicion) (celdas tablero)
listaDeFilas tablero = map (celdasEnFila tablero ) [1..(filas tablero)]

displayFilas tablero = reverse (map (displayFila tablero) (listaDeFilas tablero))

displayTablero tablero = concatMap (\displayFila -> "\n   " ++ displayFila ++ "\n") (displayFilas tablero)

instance Show Tablero where 
    show = displayTablero