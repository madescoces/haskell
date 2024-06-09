type Persona = (String, Int, String, Bool, Char)

juan :: Persona
juan = ("Juan Perez", 20, "Boedo", True, 'e')

maria :: Persona
maria = ("Maria Gonzalez", 21, "Palermo", False, 'd')

cacho :: Persona
cacho = ("Cacho Castaña", 60, "Montserrat", True, 'c')

comoTeLLamas :: Persona -> String
comoTeLLamas (nombre,_,_,_,_) = nombre

personas :: [Persona] 
personas = [juan, maria, cacho]

nombreDelUltimoVecino :: [Persona] -> String
nombreDelUltimoVecino vecinos = comoTeLLamas (last vecinos)

elMaximoEstaAlInicio :: [Integer] -> Bool
elMaximoEstaAlInicio numeros  = head numeros == maximum numeros

-- [1,2,3] ++ [4,5] Concatenar listas, number y string
-- snd ("hola", "segundo") -> "segundo"
-- tail "abcde" -> "bcde"
-- 2:[2,4,5] -> [2,3,4,5]
-- take 3 [1,2,3,4,5] -> [1,2,3]
-- drop 2 [1,2,3,4,5] -> [3,4,5]
-- "hola" !! 0 -> h
-- minimum [1,2,3,4,5] -> 1
-- sum [1,2,3,4] -> 10
-- import Data.List trae más funciones como el sort

personasAlReves :: [Persona]
personasAlReves = reverse personas

esCapicua :: Eq a => [a] -> Bool
esCapicua lista = lista == reverse lista

-- imitación de head de haskell
customHead :: [a] -> [a]
customHead [] = []
customHead (cabeza:_) = [cabeza]