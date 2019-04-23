
data Distancia = Km Float | Milhas Float deriving Show

instance Eq Distancia where
    (==) (Km x) (Km y) = (x == y)
    (==) (Milhas x) (Milhas y) = (x == y)
    (==) (Milhas x) (Km y) = (x*1.6 == y)
    (==) (Km y) (Milhas x) = (x == y/1.6)
    