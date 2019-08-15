data Estacao = Verao | Outono | Inverno | Primavera
    deriving (Eq, Ord, Enum, Show, Read)

data CondicaoClimatica = Quente | Frio | Agradavel
    deriving (Eq, Ord, Enum, Show, Read)

condicao :: Estacao -> CondicaoClimatica
-- condicao Verao = Quente
-- condicao Inverno = Frio
-- condicao x = Agradavel

condicao x | x == Verao = Quente
           | x == Inverno = Frio
           | otherwise = Agradavel

data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
    deriving (Eq, Ord, Enum, Show, Read)

dias :: Mes -> Int
dias x | x == Fevereiro = 28
       | x == Abril || x == Junho || x == Setembro || x == Novembro = 30
       | otherwise = 31

data Figura = Retangulo Float Float | Circulo Float | Triangulo Float Float Float
    deriving (Eq, Show, Read)

area :: Figura -> Float
area (Retangulo b a) = b * a
area (Circulo r) = pi * r^2
area (Triangulo a b c) = (p * (p - a) * (p - b) * (p - c)) ** 0.5
    where p = (a + b + c) / 2

eRedondo :: Figura -> Bool
eRedondo (Circulo _) = True
eRedondo x = False

perimetro :: Figura -> Float
perimetro (Circulo r) = 2 * pi * r
perimetro (Retangulo b a) = 2 * (b + a)
perimetro (Triangulo a b c) = a + b + c

type Posicao = (Float, Float)
data Figura1 = Retangulo1 Float Float Posicao | Circulo1 Float Posicao 
    deriving (Eq, Show, Read)

mover :: Posicao -> Figura1 -> Figura1
mover (x, y) (Retangulo1 b a (px, py)) = Retangulo1 b a (px + x, py + y)
mover (x, y) (Circulo1 r (px, py)) = Circulo1 r (px + x, py + y)

