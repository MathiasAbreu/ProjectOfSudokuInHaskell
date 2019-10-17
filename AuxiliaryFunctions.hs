
module AuxiliaryFunctions where
  import Data.Char

  -- Funcao exclusiva para mostrar o Sudoku. Não recomendado utilizar os demais metodos deste arquivo.
  mostrarSudoku :: [[Char]] -> IO()
  mostrarSudoku matriz = putStrLn(percorrerSudoku matriz 0)

  -- Funcao privada! Não utilizar!
  percorrerSudoku :: [[Char]] -> Int -> String
  percorrerSudoku matriz iterator
    | (iterator == 0 || iterator == 3 || iterator == 6) = " -------------------------------------\n" ++ percorrerLinha matriz iterator 0 ++ percorrerSudoku matriz (iterator + 1)
    | iterator >= 9                                     = " -------------------------------------\n"
    | otherwise                                         = percorrerLinha matriz iterator 0 ++ percorrerSudoku matriz (iterator + 1)

  -- Funcao privada! Não utilizar!
  percorrerLinha :: [[Char]] -> Int -> Int -> String
  percorrerLinha [] iterator numeroColuna = "|\n"
  percorrerLinha (head:tail) iterator numeroColuna
    | (numeroColuna /= 3) && (numeroColuna /= 6) = (percorrerColuna head 0 iterator ++ percorrerLinha tail iterator (numeroColuna + 1))
    | (numeroColuna == 3) || (numeroColuna == 6) = "|" ++ (percorrerColuna head 0 iterator ++ percorrerLinha tail iterator (numeroColuna + 1))

  -- Funcao privada! Não utilizar!
  percorrerColuna :: [Char] -> Int -> Int -> String
  percorrerColuna [] iterator linha = ""
  percorrerColuna (head:tail) iterator linha
    | iterator < linha  = percorrerColuna tail (iterator + 1) linha
    | (iterator == linha) && (head /= ' ') = "| " ++ show(digitToInt head) ++ " "
    | (iterator == linha) && (head == ' ') = "|   "
