module Knn (
    todasPredicoes
) where

import Utils


predicao :: DataSet -> Ponto -> String
predicao datasetTreino pontoTeste = fst $ foldl calcula ("", -1.0) datasetTreino
    where
        calcula acc a
            | distanciaEuclediana (fst a) (fst pontoTeste) < (snd acc) = (snd a, distanciaEuclediana (fst a) (fst pontoTeste))
            | snd acc == -1 =  (snd a, distanciaEuclediana (fst a) (fst pontoTeste))
            | otherwise = acc


todasPredicoes :: DataSet -> DataSet -> [String]
todasPredicoes datasetTreino datasetTeste = map (predicao datasetTreino) datasetTeste

