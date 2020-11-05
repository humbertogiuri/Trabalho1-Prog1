import Utils
import Classificador

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
   
   --Roda o programa para o knn
   let predicoesKnn = todasPredicoes datasetTreino datasetTeste
   let acuraciaKnn = calculaAcuracia predicoesKnn (map snd datasetTeste)
   putStr "Acuracia(vizinho): "
   putStr . show $ (acuraciaKnn * 100)
   putStrLn "%"

   --Roda o programa para centroides
   let classes = retornaClassesUnicas datasetTreino
   let vetorCentroides = centroides datasetTreino classes
   let predicoesCentroides = todasPredicoes vetorCentroides datasetTeste
   let acuraciaCentroides = calculaAcuracia predicoesCentroides (map snd datasetTeste)
   putStr "Acuracia(centroide): "
   putStr . show $ (acuraciaCentroides * 100)
   putStrLn "%"

   --Matriz de Confusao
   let matrizKnn = geraMatrizConfusao predicoesKnn (map snd datasetTeste) classes
   let matrizCentroides = geraMatrizConfusao predicoesCentroides (map snd datasetTeste) classes
   
   putStrLn "vizinho mais próximo:"
   print matrizKnn

   putStrLn "centroides:"
   print matrizCentroides
   return()