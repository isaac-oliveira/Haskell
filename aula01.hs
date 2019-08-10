menor :: Int -> Int -> Int
menor n1 n2 | n1 < n2 = n1
            | otherwise = n2

isMaior :: Float -> Float -> Int
isMaior n1 media | n1 > media = 1
                 | otherwise = 0

media :: Float -> Float -> Float -> Float
media n1 n2 n3 = (n1 + n2 + n3) / 3

menorNumero :: Int -> Int -> Int -> Int
menorNumero n1 n2 n3 = menor (menor n1 n2)  n3

acimaDaMedia :: Float -> Float -> Float -> Int
acimaDaMedia n1 n2 n3 = isMaior n1 (media n1 n2 n3) + isMaior n2 (media n1 n2 n3) + isMaior n3 (media n1 n2 n3)
