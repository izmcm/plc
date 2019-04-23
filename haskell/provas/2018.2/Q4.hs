-- (- 4) (2.5) Ao invés de uma busca linear no dicionário representado como uma lista, ele pode ser representado
-- por uma árvore binária, acelerando a busca. Implemente a função codeTree abaixo que, ao invés de receber o
-- dicionário como uma lista, o recebe como uma árvore de busca binária.

type DicionarioT = Tree Int String
data Tree chave valor = Node chave valor (Tree chave valor) (Tree chave valor) | Leaf

meuDicionarioT :: DicionarioT
meuDicionarioT = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf)
                                (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))
                                
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"

-- decodeTree meu meuDicionarioT teste ---> "a casa tinha cafe mas nao era uma cafeteria, a casa era uma sorveteria"
-- decodeTree :: DicionarioT -> String -> String

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

decodeTree :: DicionarioT -> String -> String
decodeTree _ [] = ""
decodeTree dic oStr = decodeHelp dic oStr ""

decodeHelp :: DicionarioT -> String -> String -> String
decodeHelp _ [] hStr = hStr
decodeHelp dic (s:ss) hStr | s >= '0' && s <= '9' = decodeHelp dic ss (hStr ++ match (charToInt s) dic)
                           | otherwise            = decodeHelp dic ss (hStr ++ [s]) 

match :: Int -> DicionarioT -> String
match _ Leaf = ""
match x (Node dInt dStr (f1) (f2)) | x == dInt = dStr
                                   | x > dInt  = match x f2
                                   | x < dInt  = match x f1

main = do print(decodeTree meuDicionarioT teste)