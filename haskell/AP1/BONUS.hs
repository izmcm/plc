-- [BONUS] Escreva uma função processBankOperations :: [Double] -> [(Int, Int, Int, Double)] -> [Double] 
-- que recebe uma lista com os saldos iniciais de contas bancárias e uma lista de operações bancárias e retorna
-- uma lista com os saldos atualizados das contas bancárias após as operações.

-- As operações são fornecidas como uma tupla no seguinte formato:
-- (OpCode, ContaOrigem, ContaDestino, Valor)
-- Onde OpCode pode ser:
-- 0: Crédito de Valor em ContaOrigem
-- 1: Débito de Valor em ContaOrigem
-- 2: Transferência de Valor da ContaOrigem para ContaDestino
-- ContaOrigem e ContaDestino se referem ao índice da conta bancária na lista fornecida.
-- Caso a ContaOrigem não tenha saldo para realizar uma operação, esta deverá ser ignorada.

-- Exemplo:
-- Main> processBankOperations [150.0, 50.0] [(1, 1, 0, 100.0), (2, 0, 1, 50.0), (0, 1, 0, 25.0)]
-- [100.0, 125.0]

makeOperation :: [Double] -> (Int, Int, Int, Double) -> [(Int, Int, Int, Double)] -> [Double]
makeOperation contas _ [] = contas
makeOperation contas (op,origem,destino,valor) ops = 

processBankOperations :: [Double] -> [(Int, Int, Int, Double)] -> [Double]
processBankOperations _ [] = []
processBankOperations contas (op:ops) = makeOperation contas op ops