
module BackTracking where
  import MethodsAuxiliars

  -- Metodo para testar a insercao. Não usar!
  testAdiciona :: [[Char]] -> (Int,Int) -> Char -> IO()
  testAdiciona matriz (linha,coluna) caractere = mostrarSudoku(adicionaElementoPosicao matriz (0,0) (linha,coluna) caractere)

  -- Adiciona um elemento na matriz e retorna a matriz com o elemento já adicionado.
  adicionaElementoPosicao :: [[Char]] -> (Int,Int) -> (Int,Int) -> Char -> [[Char]]
  adicionaElementoPosicao [] (linhaAtual,colunaAtual) (linha,coluna) caractere = []
  adicionaElementoPosicao (head:tail) (linhaAtual,colunaAtual) (linha,coluna) caractere
    | colunaAtual < coluna = [head] ++ (adicionaElementoPosicao tail (linhaAtual,colunaAtual + 1) (linha,coluna) caractere)
    | otherwise            = [(adicionaNaColuna head (linhaAtual,linha) caractere)] ++ tail

  -- Metodo Privado!
  adicionaNaColuna :: [Char] -> (Int,Int) -> Char -> [Char]
  adicionaNaColuna [] (linhaAtual,linha) caractere = []
  adicionaNaColuna (head:tail) (linhaAtual,linha) caractere
    | linhaAtual < linha = [head] ++ adicionaNaColuna tail (linhaAtual + 1,linha) caractere
    | otherwise          = [caractere] ++ tail
