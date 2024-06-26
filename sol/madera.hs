cuadruple :: Int -> Int
cuadruple nro = 4 * nro

-- Maderera ejercicio
largoDeListon :: Int
largoDeListon = 300

maderaCuadro :: Int -> Int
maderaCuadro = cuadruple

meAlcanza :: Int -> Bool
meAlcanza largo = maderaCuadro largo <= largoDeListon

-- funcion compuesta
puedohacerCuadroDe :: Int -> Bool
-- puedohacerCuadroDe lado = meAlcanza (maderaCuadro lado)
puedohacerCuadroDe = meAlcanza . maderaCuadro

-- Que porcentaje de listón sobraría para armar un cuadro de tamaño 50 de lado.
meSobra :: Int -> Int
meSobra lado
  | meAlcanza lado = largoDeListon - maderaCuadro lado
  | otherwise = error "No te alcanza"

porcentajeQueSobra :: Int -> Int
porcentajeQueSobra lado = meSobra lado * 100 `div` largoDeListon

esMayor :: Int -> Bool
esMayor edad = edad >= 18

esMenor :: Int -> Bool
esMenor = not . esMayor

nombreFormateado :: String -> String -> String
nombreFormateado nombre apellido = nombre ++ ", " ++ apellido

-- Que velocidad alcanza la pelota a los 5 segs
gravedad :: Float
gravedad = 9.8

-- V = t * g
velocidadCaidaLibre :: Float -> Float
velocidadCaidaLibre tiempo = tiempo * gravedad

-- D = 0.5 * t^2 * g
distanciaRecorrida :: Float -> Float
distanciaRecorrida tiempo = 1 / 2 * tiempo ** 2 * gravedad

altura :: Float
altura = 80

rebotaEnElPiso :: Float -> Bool
rebotaEnElPiso tiempo = distanciaRecorrida tiempo >= altura

-- Type Classes
elMayorDeTres :: Ord a => a -> a -> a -> a
elMayorDeTres a b c = maximum [a, b, c]

-- Pattern Matching
color :: String -> String
color "banana"  = "amarillo"
color "manzana" = "rojo"
color "limon"   = "amarillo"

horarioDeCierre :: String -> Bool -> Int
horarioDeCierre "Domingo" True = 13
horarioDeCierre "domingo" True = 13
horarioDeCierre "Sabado" False = 21
horarioDeCierre "sabado" False = 21
horarioDeCierre _ True         = 20
horarioDeCierre dia False      = 12 + length dia
