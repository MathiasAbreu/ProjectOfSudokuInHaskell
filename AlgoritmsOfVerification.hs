module AlgoritmsOfVerification where

  -- Funcao verificada! Verifica todas as três condições de inserção de um numero no sudoku.
  -- Retorna 1 se o caractere pode ser inserido.
  -- Retorna 0 se o caractere não pode ser inserido.
  verificaTotal :: [[Char]] -> Int -> Int -> Char -> Int
  verificaTotal matriz linha coluna caractere
    | (selectColuna matriz 0 coluna caractere == 1) && (selectLinha matriz linha coluna caractere == 1) && (selectSetor matriz linha coluna caractere == 1) = 1
    | otherwise = 0

  verificaTotalModific :: [[Char]] -> Int -> Int -> Char -> Int
  verificaTotalModific matriz linha coluna caractere
    | (((selectColuna matriz 0 coluna caractere) == 1) && ((selectLinhaModific matriz linha coluna caractere) == 1) && ((selectSetor matriz linha coluna caractere) == 1)) = 1
    | otherwise = 0

  retornaNumerosInChar :: Int -> Char
  retornaNumerosInChar numero
   | numero == 1 = '1'
   | numero == 2 = '2'
   | numero == 3 = '3'
   | numero == 4 = '4'
   | numero == 5 = '5'
   | numero == 6 = '6'
   | numero == 7 = '7'
   | numero == 8 = '8'
   | numero == 9 = '9'
   | otherwise   = '0'

  -- Funcao verificada! Seleciona a coluna na matriz para verificar se o elemento pode ser inserido!
  selectColuna :: [[Char]] -> Int -> Int -> Char -> Int
  selectColuna (head:tail) colunaAtual colunaDestino caractere
    | colunaAtual == colunaDestino = verificaColuna head 0 caractere
    | colunaAtual + 1 < 9          = selectColuna tail (colunaAtual + 1) colunaDestino caractere
    | otherwise                    = 0

  -- Funcao Privada!
  -- Funcao verificada! Verifica se a coluna passada como parametro, possui ou não o caractere.
  -- Retorna 1 se o caractere pode ser inserido na coluna.
  -- Retorna 0 se o caractere não pode ser inserido na coluna.
  verificaColuna :: [Char] -> Int -> Char -> Int
  verificaColuna [] n caractere = 1
  verificaColuna (head:tail) n caractere
    | head == caractere  = 0
    | otherwise          = verificaColuna tail (n + 1) caractere

  -- Funcao Verificada! Navega entre os colunas da matriz para verificar se o caractere passado existe na linha ou não.
  -- Retorna 1 se o caractere pode ser inserido na linha.
  -- Retorna 0 se o caractere não pode ser inserido na linha.
  selectLinha :: [[Char]] -> Int -> Int -> Char -> Int
  selectLinha (head:[]) linha coluna caractere                   = verificaElementoLinha head 0 linha caractere
  selectLinha (head:tail) linha coluna caractere
    | coluna > 8                                            = 1
    | ((verificaElementoLinha head 0 linha caractere) == 0) = 0
    | ((verificaElementoLinha head 0 linha caractere) == 1) = selectLinha tail linha (coluna + 1) caractere

  -- Funcao Privada!
  -- Funcao Verificada! Percorre a coluna até encontrar o elemento correspondente da linha e verificar recursivamente se o caractere pode ser inserido ou não.
  -- Retorna 1 se o caractere pode ser inserido na linha.
  -- Retorna 0 se o caractere não pode ser inserido na linha.
  verificaElementoLinha :: [Char] -> Int -> Int -> Char -> Int
  verificaElementoLinha (head:tail) iterator linha caractere
    | ((iterator == linha) && (head == caractere))  = 0
    | ((iterator == linha) && (head /= caractere))  = 1
    | iterator < linha                              = verificaElementoLinha tail (iterator + 1) linha caractere

  selectLinhaModific :: [[Char]] -> Int -> Int -> Char -> Int
  selectLinhaModific (head:[]) linha c caractere   = verificaElementoLinhaModific head 0 linha caractere
  selectLinhaModific (head:tail) linha c caractere
    | (verificaElementoLinhaModific head 0 linha caractere) == 0 = 0
    | otherwise                                                  = selectLinhaModific tail linha c caractere

  verificaElementoLinhaModific :: [Char] -> Int -> Int -> Char -> Int
  verificaElementoLinhaModific (head:tail) iterator linha caractere
    | iterator < linha                             = verificaElementoLinhaModific tail (iterator + 1) linha caractere
    | ((iterator == linha) && (head == caractere)) = 0
    | otherwise                                    = 1

  -- Vincula o setor com as margens corretas e inicia a função que irá verificar o setor.
  selectSetor :: [[Char]] -> Int -> Int -> Char -> Int
  selectSetor matriz linha coluna caractere = verificaSetor matriz 0 ((delimitaMargem linha),(delimitaMargem coluna)) ((delimitaMargem linha) + 2,(delimitaMargem coluna) + 2) caractere

  -- Funcao Privada!
  -- Função responsável por selecionar as colunas que vão ser submetidas ao validador.
  -- Retorna 1 se o caractere pode ser inserido.
  -- Retorna 0 se o caractere não pode ser inserido.
  verificaSetor :: [[Char]] -> Int -> (Int,Int) -> (Int,Int) -> Char -> Int
  verificaSetor [] iterator (linhaInicial,colunaInicial) (linhaFinal,colunaFinal) caractere                 = 1
  verificaSetor (head:tail) iterator (linhaInicial,colunaInicial) (linhaFinal,colunaFinal) caractere
    | iterator < colunaInicial                                                                                                             = verificaSetor tail (iterator + 1) (linhaInicial,colunaInicial) (linhaFinal,colunaFinal) caractere
    | (iterator == colunaInicial) && (colunaInicial <= colunaFinal) && (verificaSubcoluna head 0 (linhaInicial,linhaFinal) caractere == 1) = verificaSetor tail (iterator + 1) (linhaInicial,colunaInicial + 1) (linhaFinal,colunaFinal) caractere
    | (iterator == colunaInicial) && (colunaInicial <= colunaFinal) && (verificaSubcoluna head 0 (linhaInicial,linhaFinal) caractere == 0) = 0
    | (colunaInicial > colunaFinal)                                                                                                        = 1

  -- Funcao Privada!
  -- Função que verifica de fato, uma serie de subcolunas, para saber se tal caractere pode ou não ser inserido ali.
  -- Retorna 1 se o caractere pode ser inserido.
  -- Retorna 0 se o caractere não pode ser inserido.
  verificaSubcoluna :: [Char] -> Int -> (Int,Int) -> Char -> Int
  verificaSubcoluna matriz 9 (inicio, final) caractere = 1
  verificaSubcoluna (head:tail) iterator (inicio,final) caractere
    | iterator < inicio                            = verificaSubcoluna tail (iterator + 1) (inicio,final) caractere
    | (iterator == inicio) && (inicio > final)     = 1
    | (iterator == inicio) && (caractere /= head)  = verificaSubcoluna tail (iterator + 1) (inicio + 1,final) caractere
    | otherwise = 0

  -- Funcao Privada!
  -- Retorna um margeamento do setor para ser verificado.
  delimitaMargem :: Int -> Int
  delimitaMargem numero
    | numero <= 2                 = 0
    | (numero > 2 && numero <= 5) = 3
    | numero > 5                  = 6
