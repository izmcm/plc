makeOperation :: [Double] -> (Int, Int, Int, Double) -> [(Int, Int, Int, Double)] -> [Double]
makeOperation contas _ [] = contas
makeOperation contas (op,origem,destino,valor) ops = 

processBankOperations :: [Double] -> [(Int, Int, Int, Double)] -> [Double]
processBankOperations _ [] = []
processBankOperations contas (op:ops) = makeOperation contas op ops