module ProgFuncTP2 where
import System.IO
import Data.List (delete, nub)

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa1 :: IO()
tarefa1 = do
    putStrLn "------------------------------\n===========TAREFA 1===========\n------------------------------\n"

    --LEITURA DAS UC´S E CRIAÇÃO/LIMPEZA DO FICHEIRO
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "tarefa1.txt" WriteMode
    hClose ficheiro

    --INTRODUÇÃO DOS DADOS
    putStr "EM QUANTOS DIAS É POSSÍVEL REALIZAR-SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine

    let n_ucs = length (lines ucs)     --VER NUMERO DE DISCIPLINAS, QUE EQUIVALE AO NUMERO DE LINHAS NO FICHEIRO UCS

    --CONVERTER EM INTEIROS
    let n_dias = read dias :: Int  
    let n_salas = read salas :: Int
    if n_ucs > n_dias * n_salas then     --VERIFICAR SE É POSSÍVEL REALIZAR OS EXAMES
        putStrLn "\n-----------------------------------\nIMPOSSIVÉL REALIZAR EXAMES!\n-----------------------------------\n"
        else do
            escalonamento1 1 n_salas (lines ucs) --SE FOR POSSÍVEL, DISTRÍBUIR COMEÇANDO NO DIA 1
            putStrLn "\n-----------------------------------\nFICHEIRO CRIADO COM SUCESSO!\n-----------------------------------\n"

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa2 :: IO()
tarefa2 = do
    putStrLn "------------------------------\n===========TAREFA 2===========\n------------------------------\n"
    
    --LEITURA DAS UC´S E CRIAÇÃO/LIMPEZA DO FICHEIRO
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "tarefa2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "docs_aux2.txt" WriteMode  --RECEBE UMA CÓPIA DO FICHEIRO UCS PARA docs_aux2
    hClose ficheiro

    ficheiro <- openFile "docs_aux3.txt" WriteMode   --RECEBE UMA UC DE CADA VEZ
    hClose ficheiro

    ficheiro <- openFile "docs_aux1.txt" WriteMode   --RECEBE ANO MAIS ALTO NO FICHEIRO
    hPutStrLn ficheiro "0"
    hClose ficheiro

    copiaFicheiro (lines ucs)

    docs_aux2 <- readFile' "docs_aux2.txt"
    maiorAno (lines docs_aux2)
    let docs_aux = readFile' "docs_aux1.txt"
    maiorAnoString <- docs_aux
    let maiorAnoInt = read maiorAnoString :: Int
    
   --INTRODUÇÃO DOS DADOS
    putStr "EM QUANTOS DIAS É POSSÍVEL REALIZAR-SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine

    let n_ucs = length (lines ucs) --CONTAR NUMERO DE UCS (=LINHAS DO FICHEIRO)

    --CONVERSÃO PARA INTEIRO
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    escalonamento2 maiorAnoInt 1 n_dias n_salas (lines ucs)

    docs_aux2 <- readFile' "docs_aux2.txt"

    if null (lines docs_aux2)
        then do
            putStrLn "\n-----------------------------------\nFICHEIRO CRIADO COM SUCESSO!\n-----------------------------------\n"
            return()
        else do 
            putStrLn "\n-----------------------------------\nIMPOSSIVÉL REALIZAR EXAMES!\n-----------------------------------\n"
            return()

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa3 :: IO()
tarefa3 = do
    putStrLn "------------------------------\n===========TAREFA 3===========\n------------------------------\n" 
    ficheiro <- openFile "docs_aux1.txt" WriteMode
    hClose ficheiro
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"

    putStr "INDIQUE O NOME DA PRIMEIRA UC -> "
    disciplina <- getLine
    encontrarNome disciplina (lines ucs) (lines inscricoes) (lines alunos)
    putStr "INDIQUE O NOME DA SEGUNDA UC -> "
    disciplina <- getLine
    
    encontrarNome disciplina (lines ucs) (lines inscricoes) (lines alunos)
    docs_aux <- readFile "docs_aux1.txt"
    let tamanhoInicial = length (lines docs_aux) 
    let tamanhoFinal = length (removeDuplicados(lines docs_aux))
    let incopatibilidade = tamanhoInicial - tamanhoFinal
    putStr "\nGRAU DE INCOPATIBILIDADE = "
    print incopatibilidade
    putStrLn ""

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa4 :: IO()
tarefa4 = do
    putStrLn "------------------------------\n===========TAREFA 4===========\n------------------------------\n"

    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "tarefa1.txt" WriteMode
    hClose ficheiro

    putStr "EM QUANTOS DIAS É POSSÍVEL REALIZAR-SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine

    let n_disciplinas = length (lines ucs)

    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    if n_disciplinas > n_dias * n_salas
        then putStrLn "\n-----------------------------------\nIMPOSSIVÉL REALIZAR EXAMES!\n-----------------------------------\n"
        else do
            escalonamento4 (lines ucs) (lines inscricoes) (lines alunos) 1 n_salas (lines ucs)
            putStrLn "\n-----------------------------------\nFICHEIRO ALTERADO COM SUCESSO!\n-----------------------------------\n"


------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa5 :: IO()
tarefa5 = do
    putStrLn "------------------------------\n===========TAREFA 6===========\n------------------------------\n"
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"

    ficheiro1 <- openFile "tarefa5.txt" WriteMode
    hClose ficheiro1

    ficheiro2 <- openFile "docs_aux2.txt" WriteMode
    hClose ficheiro2

    ficheiro3 <- openFile "docs_aux3.txt" WriteMode
    hClose ficheiro3

    copiaFicheiro(lines ucs) 

    putStr "EM QUANTOS DIAS É POSSÍVEL REALIZAR-SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine

    let n_disciplinas = length (lines ucs)

    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    if n_disciplinas > n_dias * n_salas 
        then putStrLn "\n-----------------------------------\nIMPOSSIVÉL REALIZAR EXAMES!\n-----------------------------------\n"
        else do
            escalonamento5 (lines ucs) (lines inscricoes) (lines alunos) 1 n_salas
            putStrLn "\n-----------------------------------\nFICHEIRO CRIADO COM SUCESSO!\n-----------------------------------\n"

------------------------------------------------------------------------------------------------------------------------------------------------------

--TAREFA 6

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

----------FUNÇÕES TAREFA 1----------

--FAZ O ESCALONEAMENTO DAS SALAS
escalonamento1 :: Int -> Int -> [String] -> IO() 
escalonamento1 dias salas [] = return()
escalonamento1 dias salas (x:xs) = do
    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("===============DIA "++ show dias ++ "===============")
    hClose ficheiro
    imprimeSalas salas (x:xs)
    escalonamento1 (dias+1) salas (verFim salas (x:xs))

--NVÊ QUANDO ACABAM AS SALAS
verFim :: Int -> [String] -> [String]
verFim salas [] = []
verFim 0 string = string --QUANDO CHEGAR A 0 ACABA
verFim salas (x:xs) = do
    verFim (salas-1) xs

--IMPRRIMIR SALAS
imprimeSalas :: Int -> [String] -> IO()
imprimeSalas salas [] = return()
imprimeSalas 0 (x:xs) = return() --QUANDO CHEGAR A 0 ACABA
imprimeSalas salas (x:xs) = do
    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ " - "++ unwords (tail (tail(words x))))
    hClose ficheiro
    imprimeSalas (salas-1) xs


----------FUNÇÕES TAREFA 2----------

---ENCONTRA O MAIOR ANO
maiorAno :: [String] -> IO()
maiorAno [] = return()
maiorAno (x:xs) = do 

    let docs_aux = readFile' "docs_aux1.txt"
    
    docs_auxString <-  docs_aux
    let docs_auxInt = read docs_auxString :: Int

    let numero = head(tail(words x))
    let numeroInt = read numero :: Int

    if numeroInt > docs_auxInt
        then do 
            ficheiro <- openFile "docs_aux1.txt" WriteMode
            hPrint ficheiro numeroInt
            hClose ficheiro
            maiorAno xs
        else maiorAno xs

--FAZ ESCALONEAMENTO DAS SALAS
escalonamento2 :: Int -> Int -> Int -> Int -> [String] -> IO() 
escalonamento2 anoMax dias diasMax salas [] = return()
escalonamento2 anoMax dias diasMax salas (x:xs) = do

    if dias > diasMax
        then return()
        else do 

            ficheiro <- openFile "tarefa2.txt" AppendMode
            hPutStrLn ficheiro ("===============DIA "++ show dias ++ "===============")
            hClose ficheiro

            ficheiro <- openFile "docs_aux3.txt" WriteMode
            hClose ficheiro

            repetidor2 anoMax salas
        
            docs_aux3 <- readFile' "docs_aux3.txt"
            imprimeSalas2 salas (lines docs_aux3)

            docs_aux2 <- readFile' "docs_aux2.txt"
            if null (lines docs_aux2)
                then return()
                else do
                    ficheiro <- openFile "docs_aux1.txt" WriteMode
                    hPutStrLn ficheiro "0"
                    hClose ficheiro
                    maiorAno (lines docs_aux2)
                    let docs_aux = readFile' "docs_aux1.txt"
                    maiorAnoString <- docs_aux
                    let maiorAnoInt = read maiorAnoString :: Int
                    escalonamento2 maiorAnoInt (dias+1) diasMax salas (lines docs_aux2)

encontraAno :: Int -> [String] -> IO()
encontraAno _ [] = return()
encontraAno ano (x:xs) = do
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
            ficheiro <- openFile "docs_aux3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro
            copiaFicheiro xs
            return()
        else do
            ficheiro <- openFile "docs_aux2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            encontraAno ano xs

repetidor2 :: Int -> Int -> IO()
repetidor2 0 _ = return()
repetidor2 _ 0 = return()
repetidor2 anoMax salas = do 
    docs_aux2 <- readFile' "docs_aux2.txt"

    ficheiro <- openFile "docs_aux2.txt" WriteMode
    hClose ficheiro

    encontraAno anoMax (lines docs_aux2)
    repetidor2 (anoMax-1)(salas-1)


imprimeSalas2 :: Int -> [String] -> IO()
imprimeSalas2 salas [] = return()
imprimeSalas2 0 (x:xs) = return()
imprimeSalas2 salas (x:xs) = do
    ficheiro <- openFile "tarefa2.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ " - "++ unwords (tail (tail(words x))))
    hClose ficheiro
    imprimeSalas2 (salas-1) xs


----------FUNÇÕES TAREFA 3----------

removeDuplicados :: Eq a => [a] -> [a]
removeDuplicados [] = []
removeDuplicados (x:xs)
  | x `elem` xs = x : removeDuplicados (delete x xs)
  | otherwise = x : removeDuplicados xs
    
encontrarNome:: String -> [String] -> [String] -> [String]-> IO() --DESCOBRE NUMERO DA UC PELO
encontrarNome x [] y z = return()
encontrarNome input (linha:linhas) conteudo_insc conteudo_alunos = do
    let numero = head (words linha)
    if input == unwords (tail(tail(words linha)))
        then descobreAL numero conteudo_insc conteudo_alunos --
        else return()
    encontrarNome input linhas conteudo_insc conteudo_alunos  

descobreAL:: String -> [String] -> [String]-> IO() --DESCOBRE AL PELO NUMERO
descobreAL numero [] conteudo_alunos = return()
descobreAL numero (linha:linhas) conteudo_alunos = do
    
    let numero_al = head (words linha) --AL DO ALUNO
    if last (words linha) == numero --
        then do 
            ficheiro <- openFile "docs_aux1.txt" AppendMode
            hPutStrLn ficheiro (head (words linha)) 
            hClose ficheiro
        else return ()
    descobreAL numero linhas conteudo_alunos    


----------FUNÇÕES TAREFA 4----------
escalonamento4 :: [String]-> [String]-> [String]->Int -> Int -> [String] -> IO() 
escalonamento4 disciplina inscricao alunos dias salas [] = return()
escalonamento4 disciplina inscricao alunos dias salas (x:xs) = do

    ficheiro <- openFile "docs_aux1.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("===============DIA "++ show dias ++ "===============")
    hClose ficheiro

    imprimeSalas4 disciplina inscricao alunos salas (x:xs)

    ficheirodocs_aux <- readFile "docs_aux1.txt"
    let tamanhoInicial = length (lines ficheirodocs_aux) 
    let tamanhoFinal = length (removeDuplicados(lines ficheirodocs_aux)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("INCOMPATIBILIDADES = " ++ show incompativeis)
    hClose ficheiro
    
    escalonamento4 disciplina inscricao alunos (dias+1) salas (verFim salas (x:xs))
  
imprimeSalas4 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
imprimeSalas4 disciplina inscricao alunos salas [] = return()
imprimeSalas4 disciplina inscricao alunos 0 (x:xs) = return()
imprimeSalas4 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    imprimeSalas4 disciplina inscricao alunos (salas-1) xs


----------FUNÇÕES TAREFA 5----------
copiaFicheiro :: [String] -> IO()
copiaFicheiro [] = return()
copiaFicheiro (x:xs)= do 
    ficheiro <- openFile "docs_aux2.txt" AppendMode
    hPutStrLn ficheiro x
    hClose ficheiro
    copiaFicheiro xs

escalonamento5 :: [String]-> [String]-> [String]->Int -> Int  -> IO() 
escalonamento5 disciplina inscricao alunos dias salas  = do

    
    ficheiro <- openFile "docs_aux1.txt" WriteMode
    hClose ficheiro

    
    ficheiro <- openFile "tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("===============DIA "++ show dias ++ "===============")
    hClose ficheiro
   
    ficheiro <- openFile "docs_aux3.txt" WriteMode
    hClose ficheiro
    
    repetidor salas salas --REPETE O NUMERO DE SALAS ATÉ ENCONTRAR A MELHOR COMBINAÇÃO

    docs_aux3 <- readFile' "docs_aux3.txt"

    ficheiro <- openFile "docs_aux1.txt" WriteMode
    hClose ficheiro

    imprimeSalas5 disciplina inscricao alunos salas (lines docs_aux3)

    ficheirodocs_aux <- readFile "docs_aux1.txt"
    let tamanhoInicial = length (lines ficheirodocs_aux) 
    let tamanhoFinal = length (removeDuplicados(lines ficheirodocs_aux)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("INCOMPATIBILIDADES = " ++ show incompativeis)
    hClose ficheiro

    docs_aux2 <- readFile' "docs_aux2.txt"
    
    if null (lines docs_aux2)
        then return()
        else escalonamento5 disciplina inscricao alunos (dias+1) salas

     
procuraSalas :: Int -> Int-> [String] -> IO() 
procuraSalas salas _ [] = return()
procuraSalas 0 _ (x:xs) = return()
procuraSalas salas salasTotal (x:xs) = do
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"

    if salas == salasTotal
        then do 
            docs_aux3 <- openFile "docs_aux3.txt" AppendMode
            hPutStrLn docs_aux3 x 
            hClose docs_aux3

            docs_aux2 <- openFile "docs_aux2.txt" WriteMode
            hClose docs_aux2

            copiaFicheiro xs
        else do 
            ficheiro <- openFile "docs_aux1.txt" WriteMode
            hClose ficheiro
            docs_aux3 <- readFile' "docs_aux3.txt"
            let size = length (lines docs_aux3)
            loadAux2 (lines docs_aux3) size

            let disciplinas = unwords(tail(tail(words x)))
            encontrarNome disciplinas (lines ucs) (lines inscricoes) (lines alunos)

            ficheirodocs_aux <- readFile' "docs_aux1.txt"
            let tamanhoInicial = length (lines ficheirodocs_aux) 
            let tamanhoFinal = length (removeDuplicados(lines ficheirodocs_aux)) 
            let incompativeisx = tamanhoInicial - tamanhoFinal

            if null xs 
                then do 
                    docs_aux3 <- openFile "docs_aux3.txt" AppendMode
                    hPutStrLn docs_aux3 x 
                    hClose docs_aux3

                    docs_aux2 <- openFile "docs_aux2.txt" WriteMode
                    hClose docs_aux2
                    return()
                else do
   
                    ficheiro <- openFile "docs_aux1.txt" WriteMode
                    hClose ficheiro 
                    docs_aux3 <- readFile' "docs_aux3.txt"
                    let size = length (lines docs_aux3)
                    loadAux2 (lines docs_aux3) size
                   
                    let disciplinas = unwords(tail(tail(words (head xs))))
                    encontrarNome disciplinas (lines ucs) (lines inscricoes) (lines alunos)
                   
                    ficheirodocs_aux <- readFile' "docs_aux1.txt"
                    let tamanhoInicial = length (lines ficheirodocs_aux) 
                    let tamanhoFinal = length (removeDuplicados(lines ficheirodocs_aux)) 
                    let incompativeisxs = tamanhoInicial - tamanhoFinal

                    if incompativeisxs >= incompativeisx
                        then do 
                            docs_aux3 <- openFile "docs_aux3.txt" AppendMode
                            hPutStrLn docs_aux3 x 
                            hClose docs_aux3

                            docs_aux2 <- openFile "docs_aux2.txt" WriteMode
                            hClose docs_aux2

                            copiaFicheiro xs

                        else do 
                            docs_aux3 <- openFile "docs_aux3.txt" AppendMode
                            hPutStrLn docs_aux3 (head xs )
                            hClose docs_aux3

                            docs_aux2 <- openFile "docs_aux2.txt" WriteMode
                            hPutStrLn docs_aux2 x
                            hClose docs_aux2
                            
                            copiaFicheiro (tail xs)

repetidor :: Int -> Int-> IO()
repetidor 0 salasTotal = return()
repetidor salas salasTotal = do 

    docs_aux2 <- readFile' "docs_aux2.txt"
    procuraSalas salas salasTotal (lines docs_aux2)

    repetidor (salas-1) salasTotal


loadAux2 :: [String] -> Int -> IO()
loadAux2 [] size = return()
loadAux2 (x:xs) 0 = return()
loadAux2 (x:xs) size = do

    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"


    let disciplinas = unwords(tail(tail(words x)))
    encontrarNome disciplinas (lines ucs) (lines inscricoes) (lines alunos)

    loadAux2 xs (size-1)


imprimeSalas5 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
imprimeSalas5 disciplina inscricao alunos salas [] = return()
imprimeSalas5 disciplina inscricao alunos 0 (x:xs) = return()
imprimeSalas5 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    imprimeSalas5 disciplina inscricao alunos (salas-1) xs