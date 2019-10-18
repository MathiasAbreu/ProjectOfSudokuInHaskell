module BackTracking where

  import AuxiliaryFunctions
  import AlgoritmsOfVerification

  testBackTracking :: [[Char]] -> IO()
  testBackTracking matriz = mostrarSudoku(snd(backTrackingResolution matriz (0,0) 1))

  backTrackingResolution :: [[Char]] -> (Int,Int) -> Int -> (Int,[[Char]])
  backTrackingResolution matriz (x,y) 10              = (0,matriz)
  backTrackingResolution matriz (n,9) numero          = (1,matriz)
  backTrackingResolution matriz (linha,coluna) numero = if ((retornaElemento matriz (0,0) (linha,coluna)) == ' ')
    then if ((verificaTotal matriz linha coluna (retornaNumerosInChar numero)) == 1)
      then verificaSudoku(backTrackingResolution (adicionaElementoPosicao matriz (0,0) (linha,coluna) (retornaNumerosInChar numero)) (proximaPosicao (linha,coluna)) 1) matriz (linha,coluna) numero
      else (backTrackingResolution matriz (linha,coluna) (numero + 1)) --matriz (linha,coluna) numero
    else backTrackingResolution matriz (proximaPosicao (linha,coluna)) 1

  -- Essa função funciona como restart.
  verificaSudoku :: (Int,[[Char]]) -> [[Char]] -> (Int,Int) -> Int -> (Int,[[Char]])
  verificaSudoku (1,matriz) matrizAntiga (l,c) numero = (1,matriz)
  verificaSudoku (0,matriz) matrizAntiga (l,c) numero = backTrackingResolution matrizAntiga (l,c) (numero + 1)

  proximaPosicao :: (Int,Int) -> (Int,Int)
  proximaPosicao (linha,coluna)
    | (linha + 1 == 9) = (0,coluna + 1)
    | otherwise        = (linha + 1,coluna)

  -- Funcao para testar a insercao. Não usar!
  testAdiciona :: [[Char]] -> (Int,Int) -> Char -> IO()
  testAdiciona matriz (linha,coluna) caractere = mostrarSudoku(adicionaElementoPosicao matriz (0,0) (linha,coluna) caractere)

  -- Adiciona um elemento na matriz e retorna a matriz com o elemento já adicionado.
  adicionaElementoPosicao :: [[Char]] -> (Int,Int) -> (Int,Int) -> Char -> [[Char]]
  adicionaElementoPosicao [] (linhaAtual,colunaAtual) (linha,coluna) caractere = []
  adicionaElementoPosicao (head:tail) (linhaAtual,colunaAtual) (linha,coluna) caractere
    | colunaAtual < coluna = [head] ++ (adicionaElementoPosicao tail (linhaAtual,colunaAtual + 1) (linha,coluna) caractere)
    | otherwise            = [(adicionaNaColuna head (linhaAtual,linha) caractere)] ++ tail

  -- Funcao Privada!
  adicionaNaColuna :: [Char] -> (Int,Int) -> Char -> [Char]
  adicionaNaColuna [] (linhaAtual,linha) caractere = []
  adicionaNaColuna (head:tail) (linhaAtual,linha) caractere
    | linhaAtual < linha = [head] ++ adicionaNaColuna tail (linhaAtual + 1,linha) caractere
    | otherwise          = [caractere] ++ tail

  retornaElemento :: [[Char]] -> (Int,Int) -> (Int,Int) ->  Char
  retornaElemento [] (linhaAtual,colunaAtual) (linha,coluna) = 'f'
  retornaElemento (head:tail) (linhaAtual,colunaAtual) (linha,coluna)
    | colunaAtual < coluna = retornaElemento tail (linhaAtual,colunaAtual + 1) (linha,coluna)
    | otherwise            = buscaNaColuna head (linhaAtual,linha)

  buscaNaColuna :: [Char] -> (Int,Int) -> Char
  buscaNaColuna [] (linhaAtual,linhaFinal) = 'f'
  buscaNaColuna (head:tail) (linhaAtual,linhaFinal)
    | linhaAtual < linhaFinal = buscaNaColuna tail (linhaAtual + 1,linhaFinal)
    | otherwise               = head
