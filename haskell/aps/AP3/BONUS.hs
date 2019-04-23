-- [BONUS] Faça uma função faces ::  Direction -> [Command] -> Direction que informe para 
-- qual direção o robô estará voltado ao final de uma sequência de comandos 
-- (ToNorth, ToSouth, ToEast ou ToWest), assumindo que ele começa voltado para a direção ToNorth.

-- exemplo: faces ToNorth [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> ToSouth
--          faces ToNorth [Backward 2, Forward 1] ---> ToNorth
--          faces ToNorth [TurnLeft, TurnLeft, TurnLeft] ---> ToEast

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
    deriving (Eq, Show)

data Direction = ToNorth | ToSouth | ToWest | ToEast
    deriving (Eq, Show)

faces :: Direction -> [Command] -> Direction
faces pos [] = pos
faces pos (c:cs) = faces (turn pos c) cs

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

main = do print(faces ToNorth [Forward 2, TurnLeft, TurnLeft, Forward 1])
          print(faces ToNorth [Backward 2, Forward 1])
          print(faces ToNorth [TurnLeft, TurnLeft, TurnLeft])