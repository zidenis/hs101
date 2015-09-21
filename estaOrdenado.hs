module EstaOrdenado where

entrada =
  [  "8",
  "15 6 24 48 56 6 2 11",
  "11",
  "4 3 5",
  "4 5 7",
  "4 1 7",
  "0 3 6",
  "4 2 5",
  "2 4 6",
  "4 2 4",
  "1 3 13",
  "4 2 4",
  "3 3",
  "4 2 4"
  ]

---

main :: IO()
main = 
    putStr $ exeComando (drop 3 (transfInput entrada)) ((transfInput entrada !! 1)) 

-- Converte strings em inteiros
str2int :: [String] -> [Int]
str2int = map read

-- Converte a entrada
transfInput :: [String] -> [[Int]]
transfInput input = map str2int $ map words input

exeComando :: [[Int]] -> [Int] -> String
exeComando [[]] _ = ""
exeComando ([0, x, y]: xs) sequencia = exeComando xs $ trocaElem x y sequencia
exeComando ([1, x, y]: xs) sequencia = exeComando xs $ trocaVal x y sequencia
exeComando ([2, x, y]: xs) sequencia = exeComando xs $ insereElem x y sequencia
exeComando ([3, x]: xs) sequencia =  exeComando xs $ removeElem x sequencia
exeComando ([4, x, y]: xs) sequencia = (pergunta x y sequencia) ++ "\n" ++ (exeComando xs sequencia)
exeComando _ _ = ""

-- 0 X Y – Troca os elementos X e Y
trocaElem :: Int -> Int -> [Int] -> [Int]
trocaElem indexX indexY list =
    let valx = list !! (indexX-1)
        valy = list !! (indexY-1)
    in let l1 = trocaVal (indexX) valy list
           in trocaVal (indexY) valx l1

-- 1 X Y – Troca o valor do elemento na posição X por Y.
trocaVal :: Int -> Int -> [Int] -> [Int]
trocaVal index val list = x ++ (val:ys) where (x, (y:ys)) = splitAt (index-1) list

-- 2 X Y – Insere o elemento Y na posição X.
insereElem :: Int -> Int -> [Int] -> [Int]
insereElem index val list = x ++ (val:xs) where (x, xs) = splitAt (index-1) list

-- 3 X – Remove o elemento na posição X X.
removeElem :: Int -> [Int] -> [Int]
removeElem index list = x ++ ys where (x, (y:ys)) = splitAt (index-1) list

-- 4 X Y – Pergunta a estrutura dos elementos A[X..Y]
pergunta :: Int -> Int -> [Int] -> String
pergunta x y sequencia = verificaSubSeq (subSeq x y sequencia) ""

subSeq :: Int -> Int -> [Int] -> [Int]
subSeq x y sequencia = drop (x-1) $ take y sequencia

verificaSubSeq :: [Int] -> String -> String
verificaSubSeq [x] str = str
verificaSubSeq (x:y:xs) str
    | (str == "") = verificaSubSeq (y:xs) (compara x y)
    | (x == y) && (str == "ALL EQUAL") = verificaSubSeq (y:xs) str
    | (x <= y) && (str == "NON DECREASING") = verificaSubSeq (y:xs) str
    | (x <= y) && (str == "ALL EQUAL") = verificaSubSeq (y:xs) "NON DECREASING"
    | (x >= y) && (str == "NON INCREASING") = verificaSubSeq (y:xs) str
    | (x >= y) && (str == "ALL EQUAL") = verificaSubSeq (y:xs) "NON INCREASING"
    | otherwise = "NONE"

compara :: Int -> Int -> String
compara x y
    | x==y = "ALL EQUAL"
    | x<y = "NON DECREASING"
    | x>y = "NON INCREASING"
