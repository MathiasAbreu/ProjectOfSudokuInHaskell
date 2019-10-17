module Main where
  import System.IO

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
  escolherOpcaoMenu "1" = do {putStrLn "Escolha uma dificuldade:";
                              putStrLn "1. Facil";
                              putStrLn "2. Medio";
                              putStrLn "3. Dificil";
                              opcao <- getLine;
                              escolherDificuldade opcao}
  escolherOpcaoMenu "3" = putStrLn "AQUI JA CHAMA O METODO QUE VAI CRIANDO O SUDOKU"
  escolherOpcaoMenu "4" = return ()
  escolherOpcaoMenu _ = do {putStrLn "Opçao invalida, tente novamente!";
                            printaMenu}

  escolherDificuldade :: String -> IO()
  escolherDificuldade "1" = putStrLn "AQUI CHAMA O METODO QUE CRIA O JOGO FACIL"
  escolherDificuldade "2" = putStrLn "AQUI CHAMA O METODO QUE CRIA O JOGO MEDIO"
  escolherDificuldade "3" = putStrLn "AQUI CHAMA O METODO QUE CRIA O JOGO DIFICIL"
  escolherDificuldade _ = do {putStrLn "Opçao invalida, tente novamente!";
                              printaMenu}
