meuLast :: [a] -> a
meuLast [] = error "Lista vazia!"
meuLast (x:[]) = x
meuLast (x:xs) = meuLast xs

penultimo :: [a] -> a
penultimo [] = error "Lista sem penultimo"
penultimo (x:[]) = error "Lista sem penultimo"
penultimo (x:y:[]) = x
penultimo (x:xs) = penultimo xs

elementAt :: Int -> [a] -> a
elementAt 1 (x:xs) = x
elementAt a (x:xs) = elementAt (a-1) xs

meuLength :: [a] -> Int
meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs

meuReverso :: [a] -> [a]
meuReverso [] = []
meuReverso (x:xs) = meuReverso xs ++ [x]

isPalindromo :: (Eq a) => [a] -> Bool
isPalindromo (a) = (a) == meuReverso (a)

compact :: (Eq a) => [a] -> [a]
compact [] = []
compact (x:xs)
  | elem x xs = compact xs
  | otherwise = [x] ++ compact xs
