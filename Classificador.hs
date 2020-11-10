module Classificador (
    retornaClassesUnicas,
    todasPredicoes,
    centroides,
    geraMatrizConfusao,
    formataMatrizConfusao, 
    geraDocumentoSaida
) where

import Utils
import Data.List (intercalate)



{-
   Input: 1 tipo Dataset.

   Output: 1 vetor de strings.
   
   Acao: Percorre o dataset inteiro pegando apenas as classes e deixando as coordenadas de lado.
         Após isso, utiliza a funcao ja implementada removeDup para deixar apenas uma ocorrencia
         de cada classe, isso nos retorna um vetor contendo as classes que são representadas
         no dataset.
-}
retornaClassesUnicas :: DataSet -> [String]
retornaClassesUnicas dataset = removeDup . retornaClasses $ dataset
    where
        retornaClasses dataset = [classe | (_, classe) <- dataset]


{-
   Input: 1 tipo Dataset e uma string.

   Output: 1 vetor de vetores de doubles.
   
   Acao: Percorre o tipo Dataset passado e pega apenas as coordenadas cartesianas (vetor de doubles)
         e vai colocando essas coordenadas em um vetor.
-}
retornaPontosDaClasse :: DataSet -> String -> [[Double]]
retornaPontosDaClasse dataset classe = [fst dado | dado <- dataset,  snd dado == classe]


{-
   Input: 1 tipo Dataset e 1 tipo Ponto

   Output: Uma string.
   
   Acao: Passa pelos pontos do dataset calculando a distancia desses pontos com o ponto passado
         para funcao e seleciona o ponto do dataset com a menor distancia pro ponto passado.
         Apos saber o ponto do dataset com menor distancia pro ponto passado eh retornado
         a classe que esse ponto pertence.
-}
predicao :: DataSet -> Ponto -> String
predicao [] _ = error "impossivel executar predicao"
predicao _ ([],_) = error "impossivel executar predicao"
predicao datasetTreino pontoTeste = fst $ foldl calcula ("", -1.0) datasetTreino
    where
        calcula acc a
            | distanciaEuclediana (fst a) (fst pontoTeste) < (snd acc) = (snd a, distanciaEuclediana (fst a) (fst pontoTeste))
            | snd acc == -1 =  (snd a, distanciaEuclediana (fst a) (fst pontoTeste))
            | otherwise = acc


{-
   Input: 2 tipos Dataset.

   Output: 1 vetor de strings.
   
   Acao: aplica a funcao de predicao a todos os pontos do segundo dataset passado.
-}
todasPredicoes :: DataSet -> DataSet -> [String]
todasPredicoes datasetTreino datasetTeste = map (predicao datasetTreino) datasetTeste


{-
   Input: 1 vetor de vetores de doubles e 1 inteiro.

   Output: 1 vetor de doubles.
   
   Acao: Passa pelos vetor de vetores de double somando os vetores usando a funcao zipWith.
         Quando sobra apenas um vetor de doubles, que representa o somatorio de todos os vetores,
         divide todos os valores do vetor pelo tamanho total do vetor de vetores.
         No final sobra um vetor de doubles representando a media de todos os vetores
         isso eh a coordenada media de todos os pontos que representa as coordenadas
         do centroide.
-}
calculaCentroide :: [[Double]] -> Int -> [Double]
calculaCentroide [] _ = error "impossivel calcular centroide"
calculaCentroide _ 0 = error "impossivel calcular centroide"
calculaCentroide [x] i = [a / (fromIntegral i) | a <- x]
calculaCentroide (x:y:pontos) i = calculaCentroide ((zipWith (+) x y):pontos) (i + 1)    


{-
   Input: 1 tipo Dataset e 1 vetor de strings.

   Output: 1 vetor de strings.
   
   Acao: Aplica a funcao calculaCentroide para cada classe presente no vetor de strings.
-}
centroides :: DataSet -> [String] ->  DataSet
centroides dataset classes = [(calculaCentroide (retornaPontosDaClasse dataset classe) 1, classe) | classe <- classes]


{-
   Input: 3 vetores de strings.

   Output: 1 vetor de vetores de inteiros.
   
   Acao: Os 2 primeiros vetores passados para a funcao representam as classes que foram
         preditas pelo classificador e o outro sao as classes reais. O terceiro vetor
         representa as classes que podem ser classificadas. Então, ele seleciona duas classes
         das classes que podem ser preditas e confere, comparando os 2 primeiros vetores,
         a quantidade de vezes que a classe foi predita como a classe ou se foi predita como outra.
         Ele quantifica todos os resultados e coloca em uma matriz.
-}
geraMatrizConfusao :: [String] -> [String] -> [String] -> [[Int]]
geraMatrizConfusao [] _ _ = error "impossivel gerar matriz de confusao"
geraMatrizConfusao _ [] _ = error "impossivel gerar matriz de confusao"
geraMatrizConfusao _ _ [] = error "impossivel gerar matriz de confusao"
geraMatrizConfusao predicoes verdadeiros classes = 
    [ [celulaMatriz predicoes verdadeiros classe1 classe2| classe2 <- classes] 
        | classe1 <- classes]
    where
        celulaMatriz [] _ _ _ = 0
        celulaMatriz _ [] _ _ = 0
        celulaMatriz (p:predicoes) (v:verdadeiros) classe1 classe2 
            | classe1 == v && classe2 == p = 1 + celulaMatriz predicoes verdadeiros classe1 classe2
            | otherwise = celulaMatriz predicoes verdadeiros classe1 classe2


{-
    Input: 1 vetor de vetor de inteiros.

    Output: Uma string.

    Acao: Cada vetor de inteiros representa uma linha da matriz, assim essa funcao
          percorre esses vetores transformando os vetores para string e adicionando
          as virgulas, espaços e '\n' onde sao necessarios usando a funcao foldr.
-}
formataMatrizConfusao :: [[Int]] -> String
formataMatrizConfusao matriz = intercalate "\n"  (map (tail . concat) (map processaLinha matriz))
    where
        processaLinha linha = foldr (\x acc -> (',':(espacos x)++show x):acc) [] linha
        espacos x = take (3 - length(show x)) (repeat ' ')


{-
    Input: 1 FilePath e 2 vetores de vetores de inteiros (matrizes).

    Output: IO().

    Acao: Apenas transfere pro arquivo determinado e como o formato especificado as matrizes.
-}
geraDocumentoSaida :: FilePath -> [[Int]] -> [[Int]] -> IO ()
geraDocumentoSaida arquivo matrizKnn matrizCentroides = do
   writeFile arquivo "vizinho mais próximo:\n"
   appendFile arquivo (formataMatrizConfusao matrizKnn)
   appendFile arquivo "\n\n"

   appendFile arquivo "centroides:\n"
   appendFile arquivo (formataMatrizConfusao matrizCentroides)
