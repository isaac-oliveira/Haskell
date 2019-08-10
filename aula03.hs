menor :: Float -> Float -> Float
menor n1 n2 | n1 < n2 = n1
            | otherwise = n2

menorDe3 :: Float -> Float -> Float -> Float
menorDe3 n1 n2 n3 = menor (menor n1 n2) n3

maisProximoDaMedia :: Float -> Float -> Float -> Float
maisProximoDaMedia n1 n2 n3 | menorMedia == difN1 = n1
                            | menorMedia == difN2 = n2
                            | otherwise = n3
                            where menorMedia = menorDe3 difN1 difN2 difN3
                                  difN1 = abs (media - n1)
                                  difN2 = abs (media - n2)
                                  difN3 = abs (media - n3)
                                  media = (n1 + n2 + n3) / 3