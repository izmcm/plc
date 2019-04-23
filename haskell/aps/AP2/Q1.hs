-- [Q1] (25%) Dados os seguintes tipos algébricos
-- Escreva uma função evalTree :: IntTree -> Int que calcula o valor resultante das operações na árvore dada.

data Ops = SUM | SUB | MUL deriving Eq

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               
-- Exemplo:
-- evalTree (Node SUM (Node MUL (Nilt 5) (Nilt 3)) (Node SUB (Nilt 10) (Nilt 5)))
-- 20

evalTree :: IntTree -> Int
evalTree (Nilt c) = c
evalTree (Node ops f1 f2) = if ops == SUM 
                                then evalTree f1 + evalTree f2
                            else if ops == SUB 
                                then evalTree f1 - evalTree f2
                            else evalTree f1 * evalTree f2
                                
main = do print(evalTree (Node SUM (Node MUL (Nilt 5) (Nilt 3)) (Node SUB (Nilt 10) (Nilt 5))))
