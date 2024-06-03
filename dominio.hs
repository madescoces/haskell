cuadrado :: Num a => a -> a
cuadrado x = x*x


funcionCompleja :: Foldable t => Int -> Int -> t a -> Int
funcionCompleja x y z = x + cuadrado y + length z