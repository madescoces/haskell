par :: (Eq t, Num t) => t -> Bool
par 1 = False
par 2 = True
par x = par (x - 2) --procesa esto hasta que llega al resto 2 o 1 y machea con algún caso superior.

factorial :: Int -> Int
factorial n
  | n < 0 = error "Factorial no definido para números negativos"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- Genere la función sumatoria de los elementos de una lista
-- sum de list
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- Genere la función longitud de una lista
-- length de list
largo :: [Int] -> Int
largo []     = 0
largo (_:xs) = 1 + largo xs

-- Numero elevado a otro numero
-- pow
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia x y = x * potencia x (y - 1)

-- Mientras no supera el tope muestra el elemento de la lista.
-- tomar los valores menores a
tomarHasta :: Int -> [Int] -> [Int]
tomarHasta _ [] = []
tomarHasta tope (x:xs)
  | x > tope = []
  | otherwise = x : tomarHasta tope xs

-- Mostrar los n elementos primeros de la lista.
-- take de list
tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 _ = []
tomar n (x:xs)
  | n < 0 = []
  | otherwise = x : tomar (n - 1) xs

{-
aunque es infinita gracias al lazy haskell la procesa igualmente
puedo hacer funcionInfinita !! 20 para traerme su elemento 20,
o bien llamar a tomarHasta 20 (funcionInfininga algo)
esto se puede hacer directamente [1..], [1,2..], [1,2..3]
-}
funcionInfinita :: Int -> [Int]
funcionInfinita comienzo = comienzo : funcionInfinita (comienzo + 1)

-- Ejemplo de numero Biranio
binario :: Int -> Int
binario 0 = 0
binario 1 = 1
binario n = mod n 2 + binario (div n 2) * 10

-- Capicua con recursividad
esCapicua :: (Ord a) => [a] -> Bool
esCapicua [] = True
esCapicua [_] = True
esCapicua (cabeza:cola)
  | cabeza == last cola = esCapicua (init cola)
  | otherwise = False

-- Fibonacci 0 1 1 2 3 5 8 13
fibonacci :: Int -> Int
fibonacci 1      = 0
fibonacci 2      = 1
fibonacci numero = fibonacci (numero - 1) + fibonacci (numero - 2)