module Classificador (
    retornaClassesUnicas,
    todasPredicoes,
    centroides
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
calculaCentroide [x] i = [a / (fromIntegral i) | a <- x]
calculaCentroide (x:y:pontos) i = calculaCentroide ((zipWith (+) x y):pontos) (i + 1)    


--
centroides :: DataSet -> [String] ->  DataSet
centroides dataset classes = [(calculaCentroide (retornaPontosDaClasse dataset classe) 1, classe) | classe <- classes]
