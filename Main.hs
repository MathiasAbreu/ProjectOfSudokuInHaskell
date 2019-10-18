module Main where
  import System.IO
  import Generator
  import AuxiliaryFunctions
  import AlgoritmsOfVerification
  import BackTracking

  main :: IO()
  main = do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      mostraAbertura
      printaMenu

  mostraAbertura :: IO()
  mostraAbertura = do
    putStrLn "***************************************"
    putStrLn "************** SUDOKU *****************"
    putStrLn "***************************************"

  printaMenu :: IO()
  printaMenu = do
    putStrLn "1. Gerar Sudoku"
    putStrLn "2. Resolvedor de sudoku"
    putStrLn "3. Sair"
    opcao <- getLine
    escolherOpcaoMenu opcao

  escolherOpcaoMenu :: String -> IO()
  escolherOpcaoMenu "1" = do {
                              gerarSudoku
                              }
  escolherOpcaoMenu "2" = resolverSudoku
  escolherOpcaoMenu "3" = return ()
  escolherOpcaoMenu _ = do {putStrLn "";
                            putStrLn "Opçao invalida, tente novamente!";
                            printaMenu
                           }

  resolverSudoku :: IO()
  sudokuInserido = [[' ',' ','2',' ',' ','6','8',' ',' '],['3',' ',' ',' ','5',' ',' ',' ','1'],[' ','4',' ','9',' ',' ',' ','3',' '],[' ',' ',' ',' ',' ','4','5',' ','6'],[' ','5',' ',' ','6',' ',' ','1',' '],['1',' ','8','5',' ',' ',' ',' ',' '],[' ','3',' ',' ',' ','7',' ','6',' '],['8',' ',' ',' ','2',' ',' ',' ','3'],[' ',' ','7','4',' ',' ','9',' ',' ']]
  (a,sudokuResolvido) = backTrackingResolution sudokuInserido (0,0) 1
  resolverSudoku = do
    mostrarSudoku sudokuResolvido

  gerarSudoku :: IO()
  sudoku = [[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' ']]
  gerado = gerador sudoku 0 0 25
  gerarSudoku = do {
                                escolheCoordenadas gerado
                               }

  escolheCoordenadas :: [[Char]] -> IO()
  escolheCoordenadas matriz = do
    mostrarSudoku matriz
    putStr "Escolha a linha (0 para desistir):"
    linha <- readLn :: IO Int
    putStr "Escolha a coluna (0 para desistir):"
    coluna <- readLn :: IO Int
    putStr "Insira o numero: "
    numero <- readLn :: IO Int

    if (linha == 0 && coluna == 0)
      then do
        let (inteiro,matrizNova) = verificaDesistencia matriz 0 0 (retornaNumerosInChar numero)
        mostrarSudoku matrizNova
      else do
        if ((verificaTotalModific matriz (linha - 1) (coluna - 1) (retornaNumerosInChar numero)) == 1)
          then do
            let (inteiro,matrizNova) = verificaDesistencia matriz (linha - 1) (coluna - 1) (retornaNumerosInChar numero)
            if (inteiro == 1)
              then do
                mostrarSudoku matrizNova
                putStrLn "Sudoku concluido!"
              else escolheCoordenadas matrizNova
          else do
            putStrLn "Não foi possível adicionar esse número!"
            escolheCoordenadas matriz

  verificaDesistencia :: [[Char]] -> Int -> Int -> Char -> (Int,[[Char]])
  verificaDesistencia matriz 0 0 z = backTrackingResolution matriz (0,0) 1
  verificaDesistencia matriz x y z = (0,adicionaElementoPosicao matriz (0,0) (x,y) z)
