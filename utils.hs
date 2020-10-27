import System.IO ()
import Data.List ()
import Data.Char ()

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

{-
main :: IO ()
main = do
    content <- readFile "iris.csv"
    let linhas = map split $ lines content
    let linhasFormatadas = map formataLinha linhas
    mapM_ print  linhasFormatadas    
    return()
-}    