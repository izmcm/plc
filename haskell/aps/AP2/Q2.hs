-- [Q2] Dado o seguinte tipo algébrico

data IntTree = Nilt | Node Int (IntTree) (IntTree) deriving Eq

-- Escreva uma função isBST :: IntTree -> Bool que checa se uma árvore é uma árvore de busca binária. Considerar que nenhum dos elementos se repetirá.

-- Exemplos:
-- isBST (Node 5 (Node 3 Nilt Nilt) (Node 7 Nilt Nilt))
-- True
-- isBST (Node 3 (Node 5 Nilt Nilt) (Node 7 Nilt Nilt))
-- False

isBST :: IntTree -> Bool
isBST Nilt = True
isBST (Node pai (Node p1 t1 t2) (Node p2 t3 t4)) = pai > p1 && pai < p2 && isBST (Node p1 t1 t2) && isBST (Node p2 t3 t4)
isBST (Node pai (Node p1 t1 t2) Nilt) = pai > p1 && isBST (Node p1 t1 t2)
isBST (Node pai Nilt (Node p2 t3 t4)) = pai < p2 && isBST (Node p2 t3 t4)
isBST (Node pai Nilt Nilt) = True

main = do print(isBST (Node 5 (Node 3 Nilt Nilt) (Node 7 Nilt Nilt)))
          print(isBST (Node 3 (Node 5 Nilt Nilt) (Node 7 Nilt Nilt)))
