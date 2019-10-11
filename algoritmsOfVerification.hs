
-- Metodo verificado! Seleciona a coluna na matriz para verificar se o elemento pode ser inserido!
selectColuna :: [[Char]] -> Int -> Int -> Char -> Int
selectColuna (head:tail) colunaAtual colunaDestino caractere
  | colunaAtual == colunaDestino = verificaColuna head 0 caractere
  | colunaAtual + 1 < 9          = selectColuna tail (colunaAtual + 1) colunaDestino caractere
  | otherwise                    = 0

-- Metodo verificado! Verifica se a coluna passada como parametro, possui ou não o caractere.
-- Retorna 1 se o caractere pode ser inserido na coluna.
-- Retorna 0 se o caractere não pode ser inserido na coluna.
verificaColuna :: [Char] -> Int -> Char -> Int
verificaColuna [] n caractere = 1
verificaColuna (head:tail) n caractere
  | head == caractere  = 0
  | otherwise          = verificaColuna tail (n + 1) caractere

-- Metodo verificado! Navega entre os colunas da matriz para verificar se o caractere passado existe na linha ou não.
-- Retorna 1 se o caractere pode ser inserido na linha.
-- Retorna 0 se o caractere não pode ser inserido na linha.
selectLinha :: [[Char]] -> Int -> Int -> Char -> Int
selectLinha [] linha coluna caractere = 1
selectLinha (head:tail) linha coluna caractere
  | (coluna + 1) >= 9                                   = 1
  | (verificaElementoLinha head 0 linha caractere == 0) = 0
  | otherwise                                           = selectLinha tail linha (coluna + 1) caractere

-- Metodo verificado! Percorre a coluna até encontrar o elemento correspondente da linha e verificar recursivamente se o caractere pode ser inserido ou não.
-- Retorna 1 se o caractere pode ser inserido na linha.
-- Retorna 0 se o caractere não pode ser inserido na linha.
verificaElementoLinha :: [Char] -> Int -> Int -> Char -> Int
verificaElementoLinha [] iterator linha caractere = 1
verificaElementoLinha (head:tail) iterator linha caractere
  | ((iterator == linha) && (head == caractere)) = 0
  | otherwise                                    = verificaElementoLinha tail (iterator + 1) linha caractere

selectSetor :: [[Char]] -> Int -> Int -> Char -> Int
selectSetor matriz linha coluna caractere = verificaSetor matriz ((delimitaMargem linha),(delimitaMargem coluna)) ((delimitaMargem linha) + 2,(delimitaMargem coluna) + 2) caractere

verificaSetor :: [[Char]] -> (Int,Int) -> (Int,Int) -> Char -> Int
verificaSetor (head:tail) (linhaInicial,colunaInicial) (linhaFinal,colunaFinal) caractere
  | (colunaInicial <= colunaFinal) && (verificaSubcoluna head 0 (linhaInicial,linhaFinal) caractere == 1) = verificaSetor tail (linhaInicial,colunaInicial + 1) (linhaFinal,colunaFinal) caractere
  | (colunaInicial <= colunaFinal)                                                                        = 0
  | otherwise                                                                                             = 1

verificaSubcoluna :: [Char] -> Int -> (Int,Int) -> Char -> Int
verificaSubcoluna [] iterator (inicio, final) caractere = 1
verificaSubcoluna (head:tail) iterator (inicio,final) caractere
  | iterator < inicio                           = verificaSubcoluna tail (iterator + 1) (inicio,final) caractere
  | inicio > final                              = 1
  | caractere /= head                           = verificaSubcoluna tail iterator (inicio + 1,final) caractere
  | otherwise = 0

delimitaMargem :: Int -> Int
delimitaMargem numero
  | numero <= 2                 = 0
  | (numero > 2 && numero <= 5) = 3
  | numero > 5                  = 6
