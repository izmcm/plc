-- let para definição local
-- no terminal:

-- let mais10 x = x + 10 in map mais10 [1..5] == let mais10 = (\x -> x + 10) in map mais10 [1..5] == map (\x -> x + 10) [1..5] 

-- retorna uma função que será aplicada a outro número passado no final
iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter(n - 1) f).f

-- iter 10 (*2) 3
-- retorna 2^10 e depois aplica em 3

-- iter 10 (/ 2) 2000 -> 2000 dividido por 2 dez vezes
-- ter 10 ((/) 2) 2000 -> 2/2000 e dps 2/(2/2000) e assim por diante