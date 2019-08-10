toString :: [Char] -> [String]
toString cs = map string cs
              where string c = [c]

p :: [String] -> [Char] -> [String]
p ss cs = result
          where concatDupla (x, y) = x ++ y 
                result = map (concatDupla) (zip ss (toString cs))

quadrado :: [Int] -> [Int]
quadrado ns = map (pot2) ns
              where pot2 n = n ^ 2  

sumQuadrado :: [Int] -> Int
sumQuadrado ns = sum ns

verificaQuadrado :: [Int] -> Bool
verificaQuadrado ns = ns == filter (maior0) ns
                      where maior0 n = n > 0

double :: Int -> Int
double a = a * 2

twice f = f . f 

iters :: Int -> (a -> a) -> (a -> a)
iters n f = foldr g id [f | x <- [1..n]]
            where g x y = x . y


composeList :: [(a -> a)] -> (a -> a)
composeList f = foldr (.) id f

stringeira :: String -> String
stringeira s = filter (/= ' ') s

stringify :: String -> String
stringify s | takeWhile (/= ' ') s
