module MultisetList ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
-- insert elem bag = undefined
minsert :: (Ord a) => a -> [(a, Int)] -> [(a, Int)]
minsert elem [] = [(elem, 1)]
minsert elem ((x,y):xs)
    | elem == x =[(x, (y+1))] ++ xs
    | otherwise = [(x,y)] ++ minsert elem xs 

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
-- remove elem bag = undefined
mremove :: (Ord a) => a -> [(a, Int)] -> [(a, Int)]
mremove elem ((x,y):xs)
    | elem == x && y == 1 = xs
    | elem == x = [(x, (y-1))] ++ xs
    | otherwise = [(x,y)] ++ mremove elem xs

mremovequant :: (Ord a) => a -> Int -> [(a, Int)] -> [(a, Int)]
mremovequant _ 0 bag = bag
mremovequant elem quant bag = mremovequant elem (quant-1) bagAfter
    where bagAfter = mremove elem bag

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
-- search elem bag = undefined
msearch :: (Ord a) => a -> [(a, Int)] -> Int
msearch elem [] = 0
msearch elem ((x,y):xs)
    | elem == x = y
    | otherwise = msearch elem xs 

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
-- union bag1 bag2 = undefined
munion :: (Ord a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
munion a b = munion' elem a b
    where elem = union (fst (unzip a)) (fst (unzip b))

munion' :: (Ord a) => [a] -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
munion' [] _ _ = []
munion' (x:xs) a b = [(x, (max (msearch x a) (msearch x b)))] ++ munion' xs a b

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
-- intersection bag1 bag2 = undefined
minter :: (Ord a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
minter a b = minter' elem a b
    where elem = intersect (fst (unzip a)) (fst (unzip b))

minter' :: (Ord a) => [a] -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
minter' [] _ _ = []
minter' (x:xs) a b = [(x, (min (msearch x a) (msearch x b)))] ++ minter' xs a b

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 bag2 = undefined

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion bag1 bag2 = undefined

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum bag1 bag2 = undefined

{-
 - Retorna a quantidade total de elementos no Bag
-}
size bag = undefined