import Utils
import Knn

main :: IO ()
main = do
   putStr "Forneca o nome do arquivo de entrada: "
   nomeArquivoEntrada <- getLine
   
   putStr "Forneca o nome do arquivo de saida: "
   nomeArquivoSaida <- getLine
   
   putStr "Forneca o percentual de exemplos de teste: "
   porcentagem <- getLine
   
   putStr "Forneca o valor da semente para geracao randomizada: "
   seed <- getLine

   content <- readFile nomeArquivoEntrada

   let linhas = map split $ lines content
   let dataset = map formataLinha linhas
   
   let tamanhoTotal = length $ dataset
   let aleatorios = geraVetorValoresAleatorios (toInt seed) (calculaTamanhoVetorTeste (toInt porcentagem) tamanhoTotal) (tamanhoTotal)
   
   let datasetTeste = geraVetorTeste aleatorios dataset
   let datasetTreino = geraVetorTreino tamanhoTotal aleatorios dataset

   let predicoes = todasPredicoes datasetTreino datasetTeste
   
   let acuracia = calculaAcuracia predicoes (map snd datasetTeste)
   putStr "Vizinho mais proximo: "
   print (acuracia * 100)
     
   return()