-- ejercicio largo liston
largoListon :: Int
largoListon = 300

cuadruple :: Int -> Int
cuadruple numero = 4 * numero

maderaCuadro :: Int -> Int
maderaCuadro = cuadruple

meAlcanza :: Int -> Bool
meAlcanza largo = maderaCuadro largo <= largoListon

puedoHacerCuadroDe :: Int -> Bool
puedoHacerCuadroDe lado = meAlcanza (maderaCuadro lado)

puedoHacerCuadroDeDeclarativo :: Int -> Bool
puedoHacerCuadroDeDeclarativo = meAlcanza . maderaCuadro

meSobra :: Int -> Int
meSobra lado
  | meAlcanza lado = largoListon - maderaCuadro lado
  | otherwise = error "No alcanza el liston"

porcentualSobrante :: Int -> Int
porcentualSobrante lado = meSobra lado * 100 `div` largoListon

-- ejercicio composición
esMayor :: Int -> Bool
esMayor edad = edad >= 18

esMenor :: Int -> Bool
esMenor = not . esMayor

nombreFormateado :: String -> String -> String
nombreFormateado nombre apellido = apellido ++ ", " ++ nombre

-- ejercicio fisica
gravedad :: Float
gravedad = 9.8

velocidadCaidaLibre :: Float -> Float
velocidadCaidaLibre tiempo = tiempo * gravedad

distanciaRecorrida :: Float -> Float -> Float
distanciaRecorrida tiempo aceleracion = 1 / 2 * tiempo ** 2 * aceleracion

rebotaEnElPiso :: Float -> Float -> Bool
rebotaEnElPiso alturaInicial tiempo =
  distanciaRecorrida tiempo gravedad > alturaInicial

-- ejemplos de type clases
elMayorDe :: Ord a => a -> a -> a -> a
elMayorDe a b c = maximum [a, b, c]

-- fibonacci es: 0 1 1 2 3 5 8 13
fibonacciNumbersTo :: Int -> Int
fibonacciNumbersTo 1 = 0
fibonacciNumbersTo 2 = 1
fibonacciNumbersTo number
  | number >= 0 =
    fibonacciNumbersTo (number - 2) + fibonacciNumbersTo (number - 1)
  | otherwise = error "Fibonacci numbers must be positive"

data Dia
  = Lunes
  | Martes
  | Miercoles
  | Jueves
  | Viernes
  | Sabado
  | Domingo
  deriving (Show, Eq, Ord, Enum, Bounded)

-- domingo feriado 13hs
-- sabado 21hs
-- feriados 20hs
-- normal 12hs + letras dia
horarioCierre :: Dia -> Bool -> Int
horarioCierre Domingo True = 13
horarioCierre _ True       = 20
horarioCierre Sabado _     = 21
horarioCierre dia _        = 12 + length (show dia)

-- data
data Estudiante = Estudiante
  { nombre :: String
  , legajo :: Int
  , nota   :: Float
  }

pablo :: Estudiante 
pablo = Estudiante "pablo" 1 2

aprobado :: Float
aprobado = 4

estaAprobado:: Estudiante -> Bool
estaAprobado estudiante = nota estudiante >= 4