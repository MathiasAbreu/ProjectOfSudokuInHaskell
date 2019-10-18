module Generator where
  import System.Random
  import System.IO.Unsafe
  import AuxiliaryFunctions
  import Data.Char
  import AlgoritmsOfVerification
  import BackTracking
  import System.IO

  geradorChar :: Int -> Int -> Char
  geradorChar linha coluna = intToDigit (unsafePerformIO  (randomRIO (1,9)))

  geradorL :: Char -> Int
  geradorL numero = unsafePerformIO  (randomRIO (1,9))

  geradorC :: Char -> Int
  geradorC numero = unsafePerformIO  (randomRIO (1,9))

  gerador :: [[Char]] -> Int -> Int -> Int -> [[Char]]
  gerador sudoku linha coluna 0 = sudoku
  gerador sudoku linha coluna tent = do
    let numero = geradorChar linha coluna
    let lger = geradorL numero
    let cger = geradorC numero
    if ((verificaTotal sudoku lger cger numero) == 1)
      then gerador (adicionaElementoPosicao sudoku (0,0) (lger,cger) numero ) lger (cger) (tent - 1)  else gerador sudoku lger cger (tent - 1)
