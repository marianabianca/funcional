divTuple :: (Int, Int) -> Int
divTuple (x, 0) = undefined
divTuple (x, y) = quot x y

somatorio :: Int -> Int -> Int
somatorio a b = sum [a..b]

somatorioRec :: Int -> Int -> Int
somatorioRec a b
	| a == b = a
	| otherwise = a + (somatorioRec (a+1) b)

square :: Int -> Int
square a = a*a

sumSquares :: Int -> Int -> Int
sumSquares a b = square a + square b

higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b = f a + f b

hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum square

mapFilter :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
mapFilter f p xs = map p (filter f xs) 
