-- 1) Na copa do mundo de futebol, os grupos contém 4 times, e avançam para a fase seguinte os que tem maior número de pontos, sendo 3 pontos por uma
-- vitória, 1 por um empate, e zero por uma derrota.
-- Vamos representar os times e jogos com os tipos de dados e sinonimos de tipos abaixo:

data Time = Egito | Russia | Arabia | Uruguai | Ira | Marrocos | Portugal | Espanha deriving(Show, Eq)

type Jogo = (Time, Int, Int, Time)
-- Por exemplo: Egito 3 x 1 Russia será representado por (Egito, 3, 1, Russia)

jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 2, 5, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 3, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 3, Espanha), 
          (Marrocos, 2, 3, Portugal), (Marrocos, 1, 1, Espanha)]

-- a) (2.0) Defina uma função que, dado um time e uma lista de jogos, informe quantos gols aquele time fez.
-- gols :: Time -> [Jogo] -> Int

first :: (Time, Int, Int, Time) -> Time
first (a, b, c, d) = a

second :: (Time, Int, Int, Time) -> Int
second (a, b, c, d) = b

third :: (Time, Int, Int, Time) -> Int
third (a, b, c, d) = c

fourth :: (Time, Int, Int, Time) -> Time
fourth (a, b, c, d) = d

gols :: Time -> [Jogo] -> Int
gols t1 js = countGols t1 js 0

countGols :: Time -> [Jogo] -> Int -> Int
countGols _ [] x = x
countGols t1 (j:js) x 
    | t1 == (first j)  = countGols t1 js (x + (second j))
    | t1 == (fourth j) = countGols t1 js (x + (third j))
    | otherwise        = countGols t1 js x

-- b) (2.0) Defina uma função que, dado um time e uma lista de jogos, qual o seu saldo de gols naquele conjunto de jogos (gols feitos - gols tomados).
-- saldo :: Time -> [Jogo] -> Int

countAgainstGols :: Time -> [Jogo] -> Int -> Int
countAgainstGols _ [] x = x
countAgainstGols t1 (j:js) x 
    | t1 == (first j)  = countAgainstGols t1 js (x + (third j))
    | t1 == (fourth j) = countAgainstGols t1 js (x + (second j))
    | otherwise        = countAgainstGols t1 js x
    
saldo :: Time -> [Jogo] -> Int
saldo t1 js = (countGols t1 js 0) - (countAgainstGols t1 js 0)

-- c) (2.0) Defina uma função que, dado um time e uma lista de jogos, informe quantos pontos ele obteve naquele conjunto de jogos.
-- pontos :: Time -> [Jogo] -> Int

countPoints :: Time -> [Jogo] -> Int -> Int
countPoints _ [] x = x
countPoints t1 (j:js) x 
    | t1 == (first j) && (second j) > (third j)   = countPoints t1 js (x + 3)
    | t1 == (first j) && (second j) == (third j)  = countPoints t1 js (x + 1)
    | t1 == (fourth j) && (second j) < (third j)  = countPoints t1 js (x + 3)
    | t1 == (fourth j) && (second j) == (third j) = countPoints t1 js (x + 1)
    | otherwise                                   = countPoints t1 js x
    
pontos :: Time -> [Jogo] -> Int
pontos t1 js = countPoints t1 js 0

-- d) (1.0) Defina um tipo de dados para caracterizar um Grupo, que contém o nome do grupo (os grupos vão da letra 'A' à Letra 'H') e 4 times.

type Grupo = (Char, Time, Time, Time, Time)

grupo1 :: Grupo
grupo1 = ('A', Egito, Arabia, Portugal, Espanha)

-- e) (3.0)  Feito isso, defina uma função que, dado um Grupo e uma lista de jogos, retorne o par de times que estão classificados.
-- Os classificados são: os dois com maior número de pontos; em caso de empate, usa-se o saldo de gols; em caso de continuar empate usa-se 
-- o número de gols feitos (há regras adicionais, mas vamos implementar apenas essas 3).
   
-- exemplos de grupos são: Grupo A: Egito, Russia, Arabia e Uruguai; 
--                         Grupo B: Ira, Marrocos, Portugal e Espanha;
                        
-- classificados :: Grupo -> [Jogo] -> (Time, Time)

sort :: [(Time, Int, Int, Int)] -> [(Time, Int, Int, Int)]
sort [] = []
sort ((t, p, s, g):as) = sort ([(a, b, c, d) | (a, b, c, d) <- as, b > p]) ++
                         sort ([(a, b, c, d) | (a, b, c, d) <- as, b == p, c > s]) ++ 
                         sort ([(a, b, c, d) | (a, b, c, d) <- as, b == p, c == s, d > g]) ++ 
                         [(t, p, s, g)] ++ 
                         sort ([(x, y, w, z) | (x, y, w, z) <- as, y == p, w == s, z < g]) ++
                         sort ([(x, y, w, z) | (x, y, w, z) <- as, y == p, w < s]) ++
                         sort ([(x, y, w, z) | (x, y, w, z) <- as, y < p])

classificados :: Grupo -> [Jogo] -> [(Time, Int, Int, Int)]
classificados (n, t1, t2, t3, t4) js = sort [(t1, pontos t1 js, saldo t1 js, gols t1 js), (t2, pontos t2 js, saldo t2 js, gols t2 js), (t3, pontos t3 js, saldo t3 js, gols t3 js), (t4, pontos t4 js, saldo t4 js, gols t4 js)]

main = do print(gols Ira jogos1)
          print(saldo Ira jogos1)
          print(pontos Ira jogos1)
          print(classificados grupo1 jogos1)