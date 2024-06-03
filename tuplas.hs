saludar :: String -> String
saludar nombre = "Hola " ++ nombre

sumarComponentesTupla :: (Int, Int) -> Int
sumarComponentesTupla (x, 0) = x
sumarComponentesTupla (0, y) = y
sumarComponentesTupla (x, y) = x + y

-- Una tupla no necesariamente es de dos valores, puede ser una N upla
juan :: (String, Int, String, Bool)
juan = ("Juan Perez", 21, "Palermo", True)

maria :: (String, Int, String, Bool)
maria = ("Maria Gonzales", 31, "Quilmes", False)

--Y si te pudris de hacer copy paste
type Persona = (String, Int, String, Bool)

cacho :: Persona
cacho = ("Cacho Castania", 51, "Boedo", True)

irDeCompras :: (String, Int, String, Bool) -> String
irDeCompras (nombre, _, "Boedo", _) =
  saludar nombre ++ " y no Soy de San Lorenzo"
irDeCompras (nombre, _, _, False) = saludar nombre
irDeCompras (nombre, _, barrio, _) = saludar nombre ++ " de " ++ barrio

mudarseA :: String -> Persona -> Persona
mudarseA nuevoBarrio (nombre, edad, _, casa) = (nombre, edad, nuevoBarrio, casa)

cumplirAnios :: Persona -> Persona
cumplirAnios alguien = cumplirVariosAnios 1 alguien

cumplirVariosAnios :: Int -> Persona -> Persona
cumplirVariosAnios anios (nombre, edad, barrio, casa) = (nombre, edad + anios, barrio, casa)