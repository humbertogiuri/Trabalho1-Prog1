module Centroides (
    retornaClassesUnicas,
    centroides
) where

import Utils (removeDup)


retornaClassesUnicas :: [([Double], String)] -> [String]
retornaClassesUnicas dataset = removeDup . retornaClasses $ dataset
    where
        retornaClasses dataset = [classe | (_, classe) <- dataset]


--
retornaPontosDaClasse :: [([Double], String)] -> String -> [[Double]]
retornaPontosDaClasse dataset classe = [fst dado | dado <- dataset,  snd dado == classe]


--
calculaCentroide :: [[Double]] -> Int -> [Double]
calculaCentroide [x] i = [a / (fromIntegral i) | a <- x]
calculaCentroide (x:y:pontos) i = calculaCentroide ((zipWith (+) x y):pontos) (i + 1)    



centroides :: [([Double], String)] -> [String] ->  [([Double], String)]
centroides dataset classes = [(calculaCentroide (retornaPontosDaClasse dataset classe) 1, classe) | classe <- classes]
