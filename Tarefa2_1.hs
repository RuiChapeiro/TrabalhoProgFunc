module Tarefa2_1 where
import System.IO
import Data.List (delete, nub)
import Funcoes_Trabalho2

tarefa1 :: IO()
tarefa1 = do
    --LEITURA DAS UC´S E CRIAÇÃO DO FICHEIRO
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "escaloneamento_de_exames1.txt" WriteMode
    hClose ficheiro

    --INTRODUÇÃO DOS DADOS
    putStr "\n============================================\nEM QUE DIAS É POSSÍVEL REALIZAR_SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "--------------------------------------------\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine
    putStrLn "============================================\n"

    let n_ucs = length (lines ucs) --VER NUMERO DE DISCIPLINAS, QUE EQUIVALE AO NUMERO DE LINHAS NO FICHEIRO UCS

    --CONVERTER EM INTEIROS
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    if n_ucs > n_dias * n_salas  --VERIFICAR SE É POSSÍVEL REALIZAR OS EXAMES
        then putStrLn "---------------------------\nIMPOSSIVÉL ACOMODAR EXAMES!\n---------------------------\n"
        else escalonamento1 1 n_salas (lines ucs) --SE FOR POSSÍVEL, DISTRÍBUIR COMEÇANDO NO DIA 1