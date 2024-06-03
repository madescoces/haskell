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