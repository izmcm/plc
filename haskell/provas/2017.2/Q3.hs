-- 3) Um robô é controlado por 4 comandos: 
--   Left, para girar sua direção à esquerda 90 graus;
--   Right, para girar sua direção à direita em 90 graus;
--   Forward seguido de um número N, que indica um avanço de N metros.
--   Backward seguido de um número N, que indica um retrocesso de N metros.

-- Supondo que o robô comece na posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)): 
-- (3.0) faça uma função destination que informe a localização do robô após uma sequêcia de comandos.

-- Exemplo de posições/coordenadas:
-- (-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
-- (-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
-- (-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
-- (-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
-- (-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

-- data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
--   deriving (Eq, Show)
-- data Direction = North | South | West | East

-- exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
--          destination (0,0) [Backward 2, Forward 1] ---> (0,-1)
-- destination :: (Int,Int) -> [Command] -> (Int,Int)

-- RESPOSTA NA AP3