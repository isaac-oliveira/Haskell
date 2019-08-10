import Data.Char

convertAsciiMaiusculo :: Char -> Int
convertAsciiMaiusculo x = ord x

paraMaiusculo :: Char -> Char

paraMaiusculo x = if ord x > 96 && ord x < 123 then chr (convertAsciiMaiusculo x - 32) else x