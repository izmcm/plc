-- [Q1] Defina uma função classificados :: Grupo -> [Jogo] -> (Time, Time) que, dado um Grupo e uma lista de jogos, 
-- retorne o par de times que estão classificados. Os classificados são: os dois com maior número de pontos; 
-- em caso de empate, usa-se o saldo de gols; em caso de continuar empate usa-se o número de gols feitos 
-- (há regras adicionais, mas vamos implementar apenas essas 3). 

-- Exemplos de grupos são: Grupo A: Egito, Russia, Arabia e Uruguai; Grupo B: Ira, Marrocos, Portugal e Espanha.

type Grupo = (Char, Time, Time, Time, Time)

data Time = Egito | Russia | Arabia | Uruguai | Ira | Marrocos | Portugal | Espanha deriving (Show)
            
type Jogo = (Time, Int, Int, Time)

-- Por exemplo: 
-- (Egito 3 x 1 Russia) será representado por (Egito, 3, 1, Russia) e
-- o Grupo A seria ('A', Egito, Russia, Arabia, Uruguai) e 
-- o Grupo B seria ('B', Ira, Marrocos, Portugal, Espanha]
      
jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai),
          (Egito, 0, 0, Arabia), (Russia, 0, 2, Uruguai),
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai),
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha),
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha),
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]

classificados :: Grupo -> [Jogo] -> [(Time, Int, Int)]
classificados _ [] = []
classificados gp (a:as) = addPoints (toList gp) a

-- (Time, Gols a favor, Gols contra)
toList :: Grupo -> [(Time, Int, Int)]
toList (name, t1, t2, t3, t4) = [(t1, 0, 0)
                                 (t2, 0, 0),
                                 (t3, 0, 0),
                                 (t4, 0, 0)]
                                 
-- Jogo = (Time, Int, Int, Time)


main = do print(classificados ('A', Egito, Russia, Arabia, Uruguai) jogos1)