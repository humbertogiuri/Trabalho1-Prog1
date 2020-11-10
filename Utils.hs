module Utils (
   toDouble,
   toInt,
   pegaInfo,
   split,
   formataLinha,
   removeDup,
   distanciaEuclediana,
   calculaTamanhoVetorTeste,
   geraVetorValoresAleatorios,
   geraVetorTeste,
   geraVetorTreino,
   calculaAcuracia, 
   DataSet,
   Ponto
) where

import System.Random (randomRs, mkStdGen)
import System.IO

{-
Representa 1 ponto no plano cartesiano. 1 tipo que contém 1 vetor de double que representa 
as coordenadas cartesianas do ponto representado e uma string 
representado a qual classe esse ponto pertence.
-}
type Ponto = ([Double], String)

{-
1 tipo que representa um vetor de pontos, logo um dataset.
-}
type DataSet = [Ponto]


{-
   Input: Uma String.

   Output: 1 Double.
   
   Acao: Usa a funcao read para transformar uma string em 1 double.
-}
toDouble :: String -> Double
toDouble = read


{-
   Input: Uma String.

   Output: 1 inteiro.
   
   Acao: Usa a funcao read para transformar uma string em 1 inteiro.
-}
toInt :: String -> Int
toInt = read


{-
   Input: Uma String.

   Output: IO String
   
   Acao: Passa uma mensagem pro usuario no terminal e pega a proximo coisa que o usuario digitar
-}
pegaInfo :: String -> IO String
pegaInfo mensagem = do
   putStr mensagem
   hFlush stdout
   info <- getLine
   return info

{-
   Input: Uma String.

   Output: 1 vetor de Strings.
   
   Acao: Recebe uma string, representado uma linha do csv e separa todos os argumentos
         tendo em relação a vírgula para separar.
-}
split :: String -> [String]
split [] = []
split lista = split' (reverse lista) [[]]
    where
        split' [] ls = ls
        split' (x:xs) lista_aux@(l:ls)
            | x /= ',' =  split' xs ((x:l):ls)
            | otherwise = split' xs ([]:lista_aux)


{-
   Input: 1 vetor de strings.

   Output: 1 tipo Ponto.
   
   Acao: transforma o vetor de strings, que representa uma linha do csv ou seja 1 ponto,
         e transforma ele em 1 tipo ponto que contém as coordenadas cartesianas e o nome da classe.
-}
formataLinha :: [String] -> Ponto
formataLinha linha = (map toDouble $ init linha, last linha)


{-
   Input: 2 vetores de double.

   Output: 1 double.
   
   Acao: Calcula a distancia euclidiana entre 2 vetores de doubles, esses vetores
         representam as coordenadas cartesianas de um ponto, logo eh calculada a distancia
         entre dois pontos.
-}
distanciaEuclediana :: [Double] -> [Double] -> Double
distanciaEuclediana [] _ = error "ponto invalido"
distanciaEuclediana _ [] = error "ponto invalido"
distanciaEuclediana xs ys = sqrt . sum $ zipWith (\x y -> (x - y)^2) xs ys 


{-
   Input: 1 vetor de tipos Eq.

   Output: 1 vetor de tipos Eq.
   
   Acao: Percorre o vetor e remove informacoes iguais.
-}
removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
      removeD [] _ = []
      removeD (x:xs) ls
         | x `elem` ls = removeD xs ls
         | otherwise = x: removeD xs (x:ls)


{-
   Input: 2 inteiros.

   Output: 1 inteiros.
   
   Acao: Calcula quantos pontos terá o dataset de teste levando em conta a porcentagem 
         requerida e o numero total de pontos lidos do arquivo.
-}
calculaTamanhoVetorTeste :: Int -> Int -> Int
calculaTamanhoVetorTeste porcentagem tamanhoTotal = (porcentagem * tamanhoTotal) `div` 100 


{-
   Input: 3 inteiros.

   Output: 1 vetor de inteiros.
   
   Acao: gera 1 vetor de valores aleatorios e unicos para selecionar esses pontos para
         nosso dataset de teste. Esse vetor terá o tamanho que foi indicado para o dataset de teste. 
-}
geraVetorValoresAleatorios :: Int -> Int -> Int -> [Int]
geraVetorValoresAleatorios seed tamanhoTest tamanhoTotal = take (tamanhoTest) (removeDup ((randomRs (0, tamanhoTotal - 1) (mkStdGen seed) :: [Int])))


{-
   Input: 1 vetor de inteiros e 1 tipo Dataset.

   Output: 1 tipo Dataset.
   
   Acao: seleciona, dos dados lidos do arquivo csv, os Pontos que foram previamente selecionados,
         usando o vetor de aleatorios, para intergrar nosso datasetde teste.
-}
geraVetorTeste :: [Int] ->  DataSet -> DataSet
geraVetorTeste [] _ = error "impossivel gerar vetor de dados"
geraVetorTeste _ [] = error "impossivel gerar vetor de dados"
geraVetorTeste aleatorios dataset = [dataset !! x | x <- aleatorios]


{-
   Input: 1 inteiro, 1 vetor de inteiros e 1 tipo Dataset.

   Output: 1 tipo Dataset.
   
   Acao: Dos dados lidos do csv, pega aqueles que não dizem respeito ao vetor de aleatorios
         para integrar nosso vetor de treino.
-}
geraVetorTreino :: Int -> [Int] ->  DataSet -> DataSet
geraVetorTreino _ [] _ = error "impossivel gerar vetor de dados de teste"
geraVetorTreino _ _ [] = error "impossivel gerar vetor de dados treino"
geraVetorTreino tamanhoTotal aleatorios dataset = [dataset !! x | x <- [0..tamanhoTotal - 1], x `notElem` aleatorios]


{-
   Input: 2 vetores de strings.

   Output: 1 Double.
   
   Acao: Compara os dois vetores de strings e cada vez que, na mesma posicao, as informacoes
         forem iguais conta um acerto. Dividimos o numero de acertos pelo tamanho do vetor
         e temos nossa acuracia.
-}
calculaAcuracia :: [String] -> [String] -> Double
calculaAcuracia predicoes reais  = (calculaQuantidadeCorretos predicoes reais) / fromIntegral (length reais)
   where
      calculaQuantidadeCorretos _ [] = 0
      calculaQuantidadeCorretos [] _ = 0
      calculaQuantidadeCorretos (p:predicoes) (r:reais)
         | p == r = 1 + calculaQuantidadeCorretos predicoes reais
         | otherwise = 0 + calculaQuantidadeCorretos predicoes reais
