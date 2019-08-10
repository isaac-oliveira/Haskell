segundoElemento :: [Integer] -> Integer
segundoElemento xs = head (tail xs)

quantDigitos :: Int -> Int
quantDigitos x = length (show x)

pegarElemento :: Int -> [Int] -> Int
pegarElemento i xs | i <= length xs = last (take i xs)
                   | otherwise = -1

calcularMedia :: [Int] -> Float
calcularMedia xs = fromIntegral (sum xs) / fromIntegral (length xs)

ePalindromo :: Int -> Bool
ePalindromo x = xs == reverse xs
                where xs = show x

todosElementosIguais1 :: [Int] -> Bool
todosElementosIguais1 xs = maximum xs == minimum xs

somarImpare :: Int -> Int
somarImpare n = sum [1, 3..n]

combinacao :: Int -> Int -> Float
combinacao n m = fromIntegral (fatorial n) / fromIntegral ((fatorial m) * (fatorial (n - m)))

fatorial :: Int -> Int
fatorial n = product [1..n]

todosMultiplosDe5 :: [Int] -> Bool
todosMultiplosDe5 xs = 0 == sum [x `mod` 5 | x <- xs]

todosElementosIguais2 :: [Int] -> Bool
todosElementosIguais2 [] = False
todosElementosIguais2 xs = xs == [ x | x <- xs, x == primeiro]
                           where primeiro = head xs

triplicado :: [Int] -> [Int]
triplicado xs = [ x * 3 | x <- xs, x `mod` 2 /= 0] 

aproximacaoDePi :: Int -> Float
aproximacaoDePi n = (sum minusXs) * 4
                    where xs = [(x - 2, x) | x <- [3, 7 .. n * 2]]
                          minusXs = [ (1 / fromIntegral x) - (1 / fromIntegral y) | (x, y) <- xs]

--14
maisCaro :: [(String, Float)] -> Float
maisCaro xs = maximum [ x | (_, x) <- xs]

nomesMaisCaro :: [(String, Float)] -> [String]
nomesMaisCaro xs = [ nome | (nome, x) <- xs, x == valor]
                   where valor = maisCaro xs

totalItem :: String -> [(String, Float)] -> Float
totalItem nome items = sum filtrada
                       where filtrada = [ x | (nomeItem, x) <- items, nomeItem == nome]

somaDosValoresAbaixoDe5 :: [(String, Float)] -> Float
somaDosValoresAbaixoDe5 items = sum [ x | (_, x) <- items, x < 5]

somaDosValoresAcimaDe100 :: [(String, Float)] -> Float
somaDosValoresAcimaDe100 items = sum [ x | (_, x) <- items, x > 100]

totalComDesconto :: [(String, Float)] -> Float
totalComDesconto items = (valorTotal - desconto)
                       where valorTotal = sum [ x | (_, x) <- items]
                             listLeite = [ 1 | (nome, _) <- items, nome == "Leite"]
                             quantLeites = sum listLeite
                             desconto = fromIntegral (quantLeites `div` 2) * 0.5
                             