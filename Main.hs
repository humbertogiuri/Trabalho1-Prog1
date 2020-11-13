import Utils
import System.IO
import Classificador
import Text.Printf (printf)

main :: IO ()
main = do
   --Pega os dados de entrada necessarios para o programa
   nomeArquivoEntrada <- pegaInfo "Forneca o nome do arquivo de entrada: "
   nomeArquivoSaida <- pegaInfo "Forneca o nome do arquivo de saida: "
   porcentagem <- pegaInfo "Forneca o percentual de exemplos de teste: "
   seed <- pegaInfo "Forneca o valor da semente para geracao randomizada: "

   content <- readFile nomeArquivoEntrada

   --Prepara os dados em datasets de treino e teste
   let linhas = map split $ lines content
   let dataset = map formataLinha linhas
   
   let tamanhoTotal = length $ dataset
   let aleatorios = geraVetorValoresAleatorios (toInt seed) (calculaTamanhoVetorTeste (toInt porcentagem) tamanhoTotal) (tamanhoTotal)
   
   let datasetTeste = geraVetorTeste aleatorios dataset
   let datasetTreino = geraVetorTreino tamanhoTotal aleatorios dataset
   
   --Roda o programa para o knn
   let predicoesKnn = todasPredicoes datasetTreino datasetTeste
   let acuraciaKnn = calculaAcuracia predicoesKnn (map snd datasetTeste)
   printf "Acuracia(vizinho): %.2f%%\n" (acuraciaKnn * 100)

   --Roda o programa para centroides
   let classes = retornaClassesUnicas datasetTreino
   let vetorCentroides = centroides datasetTreino classes
   let predicoesCentroides = todasPredicoes vetorCentroides datasetTeste
   let acuraciaCentroides = calculaAcuracia predicoesCentroides (map snd datasetTeste)
   printf "Acuracia(centroide): %.2f%%\n" (acuraciaCentroides * 100)

   --Matriz de Confusao
   let matrizKnn = geraMatrizConfusao predicoesKnn (map snd datasetTeste) classes
   let matrizCentroides = geraMatrizConfusao predicoesCentroides (map snd datasetTeste) classes
   
   --Escreve no arquivo de saida
   geraDocumentoSaida nomeArquivoSaida matrizKnn matrizCentroides