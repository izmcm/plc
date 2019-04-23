-- 4) (2.0) faça uma função faces que informe para qual direção o robô estará voltado ao final de uma sequência de comandos 
-- (North, South, East ou West), assumindo que ele começa voltado para a direção North.

-- exemplo: faces North [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> South
--          faces North [Backward 2, Forward 1] ---> North
--          faces North [TurnLeft, TurnLeft, TurnLeft] ---> East
-- faces :: Direction -> [Command] -> Direction

-- RESPOSTA NA AP3