module Utils (
   toDouble,
   toInt,
   split,
   formataLinha,
   distanciaEuclediana,
   calculaTamanhoVetorTeste,
   geraVetorValoresAleatorios,
   geraVetorTeste,
   geraVetorTreino,
   calculaAcuracia
) where

import System.IO
import Data.List
import Data.Char
import System.Random (randomRs, mkStdGen)


--transforma uma string para double
toDouble :: String -> Double
toDouble = read


--transforma uma string para int
toInt :: String -> Int
toInt = read

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


removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
      removeD [] _ = []
      removeD (x:xs) ls
         | x `elem` ls = removeD xs ls
         | otherwise = x: removeD xs (x:ls)

--Calcula (retorna um inteiro) a quantidade de dados atribuidos para a parte de teste
calculaTamanhoVetorTeste :: Int -> Int -> Int
calculaTamanhoVetorTeste porcentagem tamanhoTotal = (porcentagem * tamanhoTotal) `div` 100 

--Gera um vetor com numeros aleatorios nao repetidos para serem usados na hora de selecionar os dados de teste
geraVetorValoresAleatorios :: Int -> Int -> Int -> [Int]
geraVetorValoresAleatorios seed tamanhoTest tamanhoTotal = take (tamanhoTest) (removeDup ((randomRs (0, tamanhoTotal - 1) (mkStdGen seed) :: [Int])))

--Coloca em um vetor de tuplas os dados que foram previamente selecionados para teste
geraVetorTeste :: [Int] ->  [([Double], String)] -> [([Double], String)]
geraVetorTeste [] _ = error "impossivel gerar vetor de teste"
geraVetorTeste _ [] = error "impossivel gerar vetor de teste"
geraVetorTeste aleatorios dataset = [dataset !! x | x <- aleatorios]


--Coloca em um vetor de tuplas os dados que foram previamente selecionados para treino
geraVetorTreino :: Int -> [Int] ->  [([Double], String)] -> [([Double], String)]
geraVetorTreino _ [] _ = error "impossivel gerar vetor de teste"
geraVetorTreino _ _ [] = error "impossivel gerar vetor de teste"
geraVetorTreino tamanhoTotal aleatorios dataset = [dataset !! x | x <- [0..tamanhoTotal - 1], x `notElem` aleatorios]


calculaAcuracia :: [String] -> [String] -> Double
calculaAcuracia predicoes reais  = (calculaQuantidadeCorretos predicoes reais) / fromIntegral (length reais)
   where
      calculaQuantidadeCorretos _ [] = 0
      calculaQuantidadeCorretos [] _ = 0
      calculaQuantidadeCorretos (p:predicoes) (r:reais)
         | p == r = 1 + calculaQuantidadeCorretos predicoes reais
         | otherwise = 0 + calculaQuantidadeCorretos predicoes reais

