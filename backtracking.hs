import algoritmsOfVerification

backtracking :: [[Char]] -> [[Char]] -> [[Char]]
backtracking [] matrizBase = []
backtracking (head:tail) matrizBase = backtracking tail 
