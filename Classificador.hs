module Classificador (
    retornaClassesUnicas,
    todasPredicoes,
    centroides,
    geraMatrizConfusao
) where

import Utils


--
retornaClassesUnicas :: DataSet -> [String]
retornaClassesUnicas dataset = removeDup . retornaClasses $ dataset
    where
        retornaClasses dataset = [classe | (_, classe) <- dataset]


--
retornaPontosDaClasse :: DataSet -> String -> [[Double]]
retornaPontosDaClasse dataset classe = [fst dado | dado <- dataset,  snd dado == classe]

--
predicao :: DataSet -> Ponto -> String
predicao [] _ = error "impossivel executar predicao"
predicao _ ([],_) = error "impossivel executar predicao"
predicao datasetTreino pontoTeste = fst $ foldl calcula ("", -1.0) datasetTreino
    where
        calcula acc a
            | distanciaEuclediana (fst a) (fst pontoTeste) < (snd acc) = (snd a, distanciaEuclediana (fst a) (fst pontoTeste))
            | snd acc == -1 =  (snd a, distanciaEuclediana (fst a) (fst pontoTeste))
            | otherwise = acc


--
todasPredicoes :: DataSet -> DataSet -> [String]
todasPredicoes datasetTreino datasetTeste = map (predicao datasetTreino) datasetTeste


--
calculaCentroide :: [[Double]] -> Int -> [Double]
calculaCentroide [] _ = error "impossivel calcular centroide"
calculaCentroide _ 0 = error "impossivel calcular centroide"
calculaCentroide [x] i = [a / (fromIntegral i) | a <- x]
calculaCentroide (x:y:pontos) i = calculaCentroide ((zipWith (+) x y):pontos) (i + 1)    


--
centroides :: DataSet -> [String] ->  DataSet
centroides dataset classes = [(calculaCentroide (retornaPontosDaClasse dataset classe) 1, classe) | classe <- classes]


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
