module Main where
  import System.IO
  import Generator
  import AuxiliaryFunctions
  import AlgoritmsOfVerification

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
    putStrLn "1. Criar novo jogo"
    putStrLn "2. Inserir Sudoku Manual"
    putStrLn "3. Resolvedor de sudoku"
    putStrLn "4. Sair"
    opcao <- getLine
    escolherOpcaoMenu opcao

  escolherOpcaoMenu :: String -> IO()
  escolherOpcaoMenu "1" = do {putStrLn "";
                              putStrLn "Escolha uma dificuldade:";
                              putStrLn "1. Facil";
                              putStrLn "2. Medio";
                              putStrLn "3. Dificil";
                              opcao <- getLine;
                              putStrLn "";
                              escolherDificuldade opcao
                              }
  escolherOpcaoMenu "2" = putStrLn "AQUI CHAMA O METODO QUE INSERE MANUALMENTE"
  escolherOpcaoMenu "3" = putStrLn "AQUI JA CHAMA O METODO QUE VAI RESOLVENDO O SUDOKU"
  escolherOpcaoMenu "4" = return ()
  escolherOpcaoMenu _ = do {putStrLn "";
                            putStrLn "Opçao invalida, tente novamente!";
                            printaMenu
                           }

  escolherDificuldade :: String -> IO()
  sudoku = [[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' '],[' ',' ',' ',' ',' ',' ',' ',' ',' ']]
  gerado = gerador sudoku 0 0 100
  escolherDificuldade "1" = do {mostrarSudoku (gerador sudoku 0 0 300);
                                escolheCoordenadas
                               }
  escolherDificuldade "2" = do {mostrarSudoku (gerador sudoku 0 0 200);
                                escolheCoordenadas
                               }
  escolherDificuldade "3" = do {mostrarSudoku (gerador sudoku 0 0 100);
                                escolheCoordenadas
                               }
  escolherDificuldade _ = do {putStrLn "Opçao invalida, tente novamente!";
                              printaMenu
                             }

  escolheCoordenadas :: [[Char]] -> IO()
  escolheCoordenadas matriz = do {putStr "Escolha a linha (0 para desistir):";

              mostrarSudoku matriz
              linha <- read(getLine);
              putStr "Escolha a coluna (0 para desistir):";
              coluna <- read(getLine);
              putStr "Insira o numero: "
              numero <- read(getLine);
              matrizNova <- escolheCoordenadas (verificaDesistencia matriz linha coluna (retornaNumerosInChar numero))

              if (fst(matrizNova) == 1)
                then putStr "Sudoku concluido!"
                else escolheCoordenadas snd(matrizNova)
    }

  verificaDesistencia :: [[Char]] Int -> Int -> (Int,[[Char]])
  verificaDesistencia matriz "0" _ = putStrLn "aqui chama o resolvedor"
  verificaDesistencia matriz _ "0" = putStrLn "aqui chama o resolvedor"
  verificaDesistencia x y = (0,adicionaElementoPosicao matriz (0,0) (x,y))
