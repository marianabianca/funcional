xor :: Bool -> Bool -> Bool
xor a b = ((a && (not b)) || ((not a) && b))

impl :: Bool -> Bool -> Bool
impl a b = ((not a) || b)

equi :: Bool -> Bool -> Bool
equi a b = ((impl a b) && (impl b a))

-- ////////////////////////////////////////////////////

square :: Int -> Int
square a = a*a

-- ////////////////////////////////////////////////////

pow :: Int -> Int -> Int
pow a 0 = 1
pow a 1 = a
pow a b = a * pow a (b-1)

-- ////////////////////////////////////////////////////

fatorial :: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial a = a * fatorial (a-1)

-- ///////////////////////////////////////////////////

isPrime :: Int -> Bool
isPrime 1 = True
isPrime a = isPrime' a (a-1)

isPrime' :: Int -> Int -> Bool
isPrime' a 1 = True
isPrime' a b = ((a `mod` b ) /= 0) && isPrime' a (b-1)

-- ///////////////////////////////////////////////////

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib a = fib (a-1) + fib(a-2)

-- ////////////////////////////////////////////////////

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

-- ///////////////////////////////////////////////////

mmc :: Int -> Int -> Int
mmc a b = (a * (b `div` (mdc a b)))

-- //////////////////////////////////////////////////

coprimo :: Int -> Int -> Bool
coprimo a b = (mdc a b) == 1

-- /////////////////////////////////////////////////

primoAnterior :: Int -> Bool -> Int
primoAnterior a True = a
primoAnterior a False = primoAnterior (a-1) (isPrime (a-1))

goldbach :: Int -> [Int]
goldbach a = [primoAnterior a False, (a - primoAnterior a False)]

-- goldbach x = [ (y,z)| y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x ] SOLUCAO DO PROFESSOR
