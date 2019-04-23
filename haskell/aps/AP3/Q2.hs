-- [Q2] Faça uma função destination :: (Int,Int) -> [Command] -> (Int,Int) que informe a 
-- localização do robô após uma sequêcia de comandos, supondo que o robô comece na 
-- posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)). 
-- Um robô é controlado por 4 comandos: 

--   Left, para girar sua direção à esquerda 90 graus;
--   Right, para girar sua direção à direita em 90 graus;
--   Forward seguido de um número N, que indica um avanço de N metros.
--   Backward seguido de um número N, que indica um retrocesso de N metros.

-- Exemplo de posições/coordenadas:
-- (-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
-- (-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
-- (-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
-- (-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
-- (-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
    deriving (Eq, Show)

data Direction = ToNorth | ToSouth | ToWest | ToEast
    deriving (Eq, Show)

-- Exemplo:
-- destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1]
-- > (0,1)
-- destination (0,0) [Backward 2, Forward 1]
-- > (0,-1)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination loc c = d2 loc c ToNorth
    
d2 :: (Int,Int) -> [Command] -> Direction -> (Int,Int)
d2 loc [] pos = loc
d2 loc (c:cs) pos
    | c == TurnLeft || c == TurnRight = d2 loc cs (turn pos c)
    | otherwise = d2 (execCommand pos loc c) cs pos

execCommand :: Direction -> (Int, Int) -> Command -> (Int, Int)
execCommand pos (x, y) (Forward d)
    | pos == ToNorth = (x, y+d)
    | pos == ToSouth = (x, y-d)
    | pos == ToWest = (x-d, y)
    | pos == ToEast = (x+d, y)
    | otherwise = (x, y)
execCommand pos (x, y) (Backward d)
    | pos == ToNorth = (x, y-d)
    | pos == ToSouth = (x, y+d)
    | pos == ToWest = (x+d, y)
    | pos == ToEast = (x-d, y)
    | otherwise = (x, y)

turn :: Direction -> Command -> Direction
turn pos c 
    | c == TurnLeft && pos == ToNorth = ToWest
    | c == TurnLeft && pos == ToSouth = ToEast
    | c == TurnLeft && pos == ToEast = ToNorth
    | c == TurnLeft && pos == ToWest = ToSouth
    | c == TurnRight && pos == ToNorth = ToEast
    | c == TurnRight && pos == ToSouth = ToWest
    | c == TurnRight && pos == ToEast = ToSouth
    | c == TurnRight && pos == ToWest = ToNorth
    | otherwise = pos

main = do print(destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1])
          print(destination (0,0) [Backward 2, Forward 1])