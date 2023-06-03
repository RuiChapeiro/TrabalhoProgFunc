module Tarefa2_2 where
import System.IO
import Data.List (delete, nub)
import Funcoes_Trabalho2

tarefa2 :: IO()
tarefa2 = do 
    ucs <- readFile "ucs.txt"

    ficheiro <- openFile "escaloneamento_de_exames2.txt" WriteMode
    hClose ficheiro
    --fazer uma copia de ucs para ficheiro 2 
    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro
    --descobrir qual o ano mais alto escrever no ficheiro 1

    copyFile (lines ucs)

    suporte2 <- readFile' "Suporte2.txt"
    maiorAno (lines suporte2)
    let suporte = readFile' "Suporte.txt"
    maiorAnoString <- suporte
    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
    

    --Pedir input
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    --linhas no ficheiro-
    let n_disciplinas = length (lines ucs) -- numero de disciplinas é o numero de linhas no ficheiro
    --convercao-- para int
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    escalonamento2 maiorAnoInt 1 n_dias n_salas (lines ucs)

    suporte2 <- readFile' "Suporte2.txt"

    if null (lines suporte2)
        then return()
        else do 
            putStrLn "Tempo insuficiente para acomodar todos os exames"
            return()
