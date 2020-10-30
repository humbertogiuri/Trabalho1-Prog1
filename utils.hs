import System.IO ()
import Data.List ()
import Data.Char ()
import System.Random (randomRs, mkStdGen)

--transforma umas string para double
toDouble :: String -> Double
toDouble = read

--separa as linhas do csv 
split :: String -> [String]
split [] = []
split lista = split' (reverse lista) [[]]
    where
        split' [] ls = ls
        split' (x:xs) lista_aux@(l:ls)
            | x /= ',' =  split' xs ((x:l):ls)
            | otherwise = split' xs ([]:lista_aux)


--Transforma um vetor de strings para uma tupla de vetores de Double e uma string
formataLinha :: [String] -> ([Double], String)
formataLinha linha = (map toDouble $ init linha, last linha)


--calcula a distancia euclidiana entre dois pontos
distanciaEuclediana :: [Double] -> [Double] -> Double
distanciaEuclediana [] _ = error "ponto invalido"
distanciaEuclediana _ [] = error "ponto invalido"
distanciaEuclediana xs ys = sqrt . sum $ zipWith (\x y -> (x - y)^2) xs ys 

tem :: Eq a => a -> [a] -> Bool
tem _ [] = False
tem y (x:xs)
   | x == y = True
   | otherwise = tem y xs


removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | tem x ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)


calculaTamanhoVetorTeste :: Int -> Int -> Int
calculaTamanhoVetorTeste porcentagem tamanhoTotal = (porcentagem * tamanhoTotal) `div` 100 


geraVetorValoresAleatorios :: Int -> Int -> Int -> [Int]
geraVetorValoresAleatorios seed tamanhoTest tamanhoTotal = take (tamanhoTest) (removeDup ((randomRs (1, tamanhoTotal) (mkStdGen seed) :: [Int])))

geraVetorTeste :: [Int] ->  [([Double], String)] -> [([Double], String)]
geraVetorTeste [] _ = error "impossivel gerar vetor de teste"
geraVetorTeste _ [] = error "impossivel gerar vetor de teste"
geraVetorTeste aleatorios dataset = [dataset !! x | x <- aleatorios]

geraVetorTrain :: Int -> [Int] ->  [([Double], String)] -> [([Double], String)]
geraVetorTrain _ [] _ = error "impossivel gerar vetor de teste"
geraVetorTrain _ _ [] = error "impossivel gerar vetor de teste"
geraVetorTrain tamanhoTotal aleatorios dataset = [dataset !! x | x <- [0..tamanhoTotal - 1], x `notElem` aleatorios]

{- 
separaPorInd :: [Int] -> [a] -> ([a], [a])
separaPorInd lind base = (base1, base2)
   where
      base1 = [base!!i|i<-[0..(length(base)-1)], 
         not (elem i lind)] 
      base2 = [base!!i|i <- lind]
-}

main :: IO ()
main = do
   content <- readFile "iris.csv"
   let linhas = map split $ lines content
   let dataset = map formataLinha linhas
   let tamanhoTotal = length $ dataset
   let aleatorios = geraVetorValoresAleatorios 42 (calculaTamanhoVetorTeste 20 tamanhoTotal) (tamanhoTotal)
   let datasetTeste = geraVetorTeste aleatorios dataset
   let datasetTrain = geraVetorTrain tamanhoTotal aleatorios dataset
   mapM_ print  datasetTrain
   print . length $ datasetTrain
   return()


