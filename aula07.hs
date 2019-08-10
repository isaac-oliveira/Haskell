type Picture = [String]

rotate90 :: Picture -> Picture
rotate90 [] = []
rotate90 (ln:pic) = zipWith snoc (rotate90 pic) ln
                    where snoc ln1 pic = ln1 ++ [pic]

replicateAll :: Int -> [a] -> [a]
replicateAll n list = concat (map (replicate n) list )

scale :: Picture -> Int -> Picture
scale pic n = map (replicateAll n) (replicateAll n pic)

type Rep = (Int, Int, [(Int, Int)])

flipV :: Rep -> Rep
flipV (c, l, ps) = (c, l, map (invertV) ps)
                where invertV (i, j) = (i, (c - 1) - j)

flipH :: Rep -> Rep
flipH (c, l, ps) = (c, l, map (invertH) ps )
                where invertH (i, j) = ((l - 1) - i, j)

sideBySide :: Rep -> Rep -> Rep
sideBySide (m1, n1, ps1) (m2, n2, ps2) = (m1 + m2, n1 + n2, ps1 ++ ps3)
                                    where  ps3 = map (somar) ps2
                                           somar (x, y) = (x, y + m1)

algo :: [Int] -> [Int] -> [Int]
algo xs ys = zipWith (soma)  xs ys
          where soma x y = x + y

f :: [Int] -> [Int]
f xs = filter (zero) xs
       where zero x = x /= 0

algo2 :: [Int] -> Int
algo2 xs = foldr (soma) 0 xs
         where soma a b = a + b

algo3 :: [Int] -> [Int] -> [Int]
algo3 xs ys = zipWith (\a b -> a + b) xs ys

multiplicar :: Int -> Int -> Int
multiplicar x y = x * y

double :: Int -> Int
double = multiplicar 2

iguais :: (Int -> Int) -> Int -> Bool
iguais f n = (dropWhile (== l) lista) == []
       where lista = map f [0..n]
             l = head lista

qPar :: [Int] -> Int
qPar xs = foldr1 (*) ys
       where ys = map (fun) xs
             fun x = x*x


