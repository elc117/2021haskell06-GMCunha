-- Prática 06 de Haskell
-- Nome: Guilherme Medeiros da Cunha

ends :: [Int] -> [Int]
ends (x:xs) = x:last xs:[]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = 2*x : deduzame xs

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs 
  else deduzame2 xs

geraTabela :: Int -> [(Int,Int)]
geraTabela 1 = [(1, 1)]
geraTabela n = (n, n^2):geraTabela (n-1)

contido :: Char -> String -> Bool
contido _ [] = False
contido c (x:xs) = c == x || contido c xs

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate (x:xs) = (fst x + 2, snd x + 2):translate xs

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if (length x) > 5 then 1 + countLongs xs else countLongs xs

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if (length x) > 5 then x:onlyLongs xs else onlyLongs xs