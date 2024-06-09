add :: Int -> Int -> Int
add x y = x + y

addFive :: Int -> Int
addFive = add 5

double :: Int -> Int
double x = 2 * x

increment :: Int -> Int
increment x = x + 1

incrementAndDouble :: Int -> Int
incrementAndDouble = double . increment

{-
  Una función que recibe varios parámetros si solo le paso 1 se va a quedar esperando los restantes
  filter (2>) [1,2,3] = 1 --> (2>_) se queda esperando el segundo parámetro
  cualquier función de más de un párametro si paso 1 solo va a devolver otra función
-}

mejor :: Ord a => (t -> a) -> t -> t -> t
mejor f x y
  | f x > f y = x
  | otherwise = y

-- se puede aplicar parcialmente la suma por ejemplo --> mejor (1+) 3 (-123)