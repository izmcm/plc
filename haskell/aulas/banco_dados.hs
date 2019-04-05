type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr.Norrell"),
               ("Fernando","Duna")]
               
-- -- Atualizações -- --
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd ps li = (ps, li):bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver ((a,b):abs) ps li | (a == ps) && (b == li) = abs
                           | otherwise = (a,b):(devolver abs ps li)

-- -- Consultas -- --
livrosRecursive :: BancoDados -> Pessoa -> [Livro]
livrosRecursive [] _ = []
livrosRecursive ((a,b):abs) ps | a == ps = (b:(livrosRecursive abs ps))
                               | otherwise = livrosRecursive abs ps

livros :: BancoDados -> Pessoa -> [Livro]
livros bd ps = [liv|(pess, liv) <- bd, ps == pess]

emprestadoRecursive :: BancoDados -> Livro -> Bool
emprestadoRecursive [] _ = False
emprestadoRecursive ((a,b):abs) li | b == li = True
                                   | otherwise = (emprestadoRecursive abs li)

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = [pess|(pess, liv) <- bd, liv == l] /= []

emprestimosRecursive :: BancoDados -> Livro -> [Pessoa]
emprestimosRecursive [] _ = []
emprestimosRecursive ((a,b):abs) li | b == li = (a:(emprestimosRecursive abs li))
                                    | otherwise = (emprestimosRecursive abs li)

qtdEmprestimosRecursive :: BancoDados -> Pessoa -> Int
qtdEmprestimosRecursive [] _ = 0
qtdEmprestimosRecursive ((a,b):abs) ps | a == ps = 1 + (qtdEmprestimosRecursive abs ps)
                                       | otherwise = (qtdEmprestimosRecursive abs ps)


main = do print(emprestar baseExemplo "Ana" "Harry Potter")
          print(emprestado (emprestar baseExemplo "Ana" "Harry Potter") "Harry Potter")
          print(emprestado baseExemplo "Harry Potter")