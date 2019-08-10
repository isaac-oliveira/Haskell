xOr :: Bool -> Bool -> Bool
xOr x y = x /= y

exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr True False = True
exOr False True = True
exOr False False = True

mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m == n) && (n == p))