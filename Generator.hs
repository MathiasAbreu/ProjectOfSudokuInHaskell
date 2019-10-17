module Generator where
  import System.Random
  import System.IO.Unsafe
  import AuxiliaryFunctions
  import Data.Char
  import AlgoritmsOfVerification
  import BackTracking
  import System.IO

  geradorChar :: Char
  m = randomRIO (1,9)
  numero = unsafePerformIO m
  n = intToDigit numero
  geradorChar = n

  gerador :: [[Char]] -> Int -> Int -> [[Char]]
  gerador sudoku linha 9 = gerador sudoku (linha + 1) 0
  gerador sudoku 9 coluna = sudoku
  gerador sudoku linha coluna
    | (verificaTotal sudoku linha coluna geradorChar == 1) = gerador (adicionaElementoPosicao sudoku (0,0) (linha,coluna) (intToDigit numero) ) linha (coluna + 1)
    | otherwise = gerador sudoku linha coluna
