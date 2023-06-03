module Prog_Func_TP2 where
import System.IO
import Data.List (delete, nub)

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa1 :: IO()
tarefa1 = do
    --LEITURA DAS UC´S E CRIAÇÃO DO FICHEIRO
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "tarefa1.txt" WriteMode
    hClose ficheiro

    --INTRODUÇÃO DOS DADOS
    putStr "\n==================================================\nEM QUANTOS DIAS É POSSÍVEL REALIZAR-SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "--------------------------------------------------\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine
    putStrLn "==================================================\n"

    let n_ucs = length (lines ucs)     --VER NUMERO DE DISCIPLINAS, QUE EQUIVALE AO NUMERO DE LINHAS NO FICHEIRO UCS

    --CONVERTER EM INTEIROS
    let n_dias = read dias :: Int  
    let n_salas = read salas :: Int
    if n_ucs > n_dias * n_salas then     --VERIFICAR SE É POSSÍVEL REALIZAR OS EXAMES
        putStrLn "------------------------------\nIMPOSSIVÉL REALIZAR EXAMES!\n------------------------------\n"
        else do
            escalonamento1 1 n_salas (lines ucs) --SE FOR POSSÍVEL, DISTRÍBUIR COMEÇANDO NO DIA 1
            putStrLn "------------------------------\nFICHEIRO CRIADO COM SUCESSO!\n------------------------------\n"

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa2 :: IO()
tarefa2 = do
    --LEITURA DAS UC´S E CRIAÇÃO DO FICHEIRO
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "tarefa2.txt" WriteMode
    hClose ficheiro

    --FAZ UMA CÓPIA DO FICHEIRO UCS PARA SUPORTE2
    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro

    --ESCREVE UMA UC DE CADA VEZ
    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro

    --ESCREVE ANO MAIS ALTO NO 1ª FICHEIRO
    ficheiro <- openFile "Suporte1.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro

    copyFile (lines ucs)

    suporte2 <- readFile' "Suporte2.txt"
    maiorAno (lines suporte2)
    let suporte = readFile' "Suporte1.txt"
    maiorAnoString <- suporte
    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
    
   --INTRODUÇÃO DOS DADOS
    putStr "\n==================================================\nEM QUANTOS DIAS É POSSÍVEL REALIZAR-SE OS EXAMES?\n-> "
    dias <- getLine
    putStr "--------------------------------------------------\nQUANTAS SALAS ESTÃO DISPONIVÉIS DIARIAMENTE?\n-> "
    salas <- getLine
    putStrLn "==================================================\n"

    let n_ucs = length (lines ucs) --CONTAR NUMERO DE UCS (=LINHAS DO FICHEIRO)

    --CONVERSÃO PARA INTEIRO
    let n_dias = read dias :: Int
    let n_salas = read salas :: Int

    escalonamento2 maiorAnoInt 1 n_dias n_salas (lines ucs)

    suporte2 <- readFile' "Suporte2.txt"

    if null (lines suporte2)
        then do
            putStrLn "------------------------------\nFICHEIRO CRIADO COM SUCESSO!\n------------------------------\n"
            return()
        else do 
            putStrLn "------------------------------\nIMPOSSIVÉL REALIZAR EXAMES!\n------------------------------\n"
            return()

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa3 :: IO()
tarefa3 = do 
    ficheiro <- openFile "Suporte1.txt" WriteMode
    hClose ficheiro
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"
    putStrLn "indique o nome da disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines ucs) (lines inscricoes) (lines alunos)
    putStrLn "indique o nome da outra disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines ucs) (lines inscricoes) (lines alunos)
    suporte <- readFile "Suporte1.txt"
    let tamanhoInicial = length (lines suporte) 
    let tamanhoFinal = length (remDup(lines suporte))  --remDUP -> REMOVE REPETIDOS
    let imcopatibilidade = tamanhoInicial - tamanhoFinal
    print imcopatibilidade

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa4 :: IO()
tarefa4 = do 
    --LER FICHEIROS
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"
    --limpar ficheiros
    ucs <- readFile "ucs.txt"
    ficheiro <- openFile "tarefa1.txt" WriteMode
    hClose ficheiro

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
    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else escalonamento4 (lines ucs) (lines inscricoes) (lines alunos) 1 n_salas (lines ucs)

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa5 :: IO()
tarefa5 = do 
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"

    ficheiro <- openFile "tarefa5.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro


    copyFile(lines ucs) --fazer uma copia para ficheiro suporte 2
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

    if n_disciplinas > n_dias * n_salas  -- se numero de disciplinas maior que dias livres * salas impossivel
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else escalonamento5 (lines ucs) (lines inscricoes) (lines alunos) 1 n_salas 

------------------------------------------------------------------------------------------------------------------------------------------------------

tarefa6 :: IO()
tarefa6 = do 
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"


    ficheiro <- openFile "tarefa6.txt" WriteMode
    hClose ficheiro

    ficheiro2 <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro2 

    formatacao6 (lines ucs) (lines inscricoes) (lines alunos) (lines ucs)
    -- texto formatado no ficheiro suporte 2 quantidade nome disciplina

    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    putStrLn "indique a capacidade das salas no formato [Sala 1,Sala 2,...]"
    lotacao <- getLine
    let lotacaoInt = read lotacao :: [Int]

    let n_dias = read dias :: Int
    let n_salas = read salas :: Int
    let lotacaoReverse = reverse lotacaoInt
    if length (lotacaoReverse) /= n_salas
        then do 
            putStrLn("lotacao diferente do numero de salas")
            return()
        else do 
            conteudoSuporte2 <- readFile' "Suporte2.txt"

            ficheiro <- openFile "Suporte.txt" WriteMode
            hPutStrLn ficheiro (head (lines conteudoSuporte2))  --escrever primeira linha do suporte2 no suporte
            hClose ficheiro
            
            escalonamento6 1 n_dias n_salas lotacaoReverse (lines conteudoSuporte2)

            suporte2 <- readFile' "Suporte2.txt"
            if suporte2 /= []
                then putStrLn ("Insuficiente para acomodar todos os exames")
                else return()

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

----------FUNÇÕES TAREFA 1----------

--FAZ O ESCALONEAMENTO DAS SALAS
escalonamento1 :: Int -> Int -> [String] -> IO() 
escalonamento1 dias salas [] = return()
escalonamento1 dias salas (x:xs) = do
    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("==========DIA "++ show dias ++ "==========")
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

    let suporte = readFile' "Suporte1.txt"
    
    suporteString <-  suporte
    let suporteInt = read suporteString :: Int

    let numero = head(tail(words x))
    let numeroInt = read numero :: Int

    if numeroInt > suporteInt
        then do 
            ficheiro <- openFile "Suporte1.txt" WriteMode
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
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            ficheiro <- openFile "Suporte3.txt" WriteMode
            hClose ficheiro --limpar ficheiro3

            --funcao que seleciona disciplinas para ficheiro3 e apaga o ficheiro 2


            repeater2 anoMax salas
            --ficheiro3 a zero
            
        

            suporte3 <- readFile' "Suporte3.txt"
            imprimeSalas2 salas (lines suporte3)--print nos ficheiros do 3

            suporte2 <- readFile' "Suporte2.txt"
            if null (lines suporte2)
                then return()
                else do
                    ficheiro <- openFile "Suporte1.txt" WriteMode
                    hPutStrLn ficheiro "0"
                    hClose ficheiro
                    maiorAno (lines suporte2)
                    let suporte = readFile' "Suporte1.txt"
                    maiorAnoString <- suporte
                    let maiorAnoInt = read maiorAnoString :: Int -- maior ano no ficheiro
                    escalonamento2 maiorAnoInt (dias+1) diasMax salas (lines suporte2)

--NAO SEI
finder :: Int -> [String] -> IO()
finder _ [] = return()
finder ano (x:xs) = do --ano a encontrar, ficheiro2
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
            ficheiro <- openFile "Suporte3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro --limpar ficheiro3
            copyFile xs
            return()
        else do
            ficheiro <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            finder ano xs

 
repeater2 :: Int -> Int -> IO()
repeater2 0 _ = return()
repeater2 _ 0 = return()
repeater2 anoMax salas = do --ano maximo a procurar e numero de salas a preencher
    suporte2 <- readFile' "Suporte2.txt"

    ficheiro <- openFile "Suporte2.txt" WriteMode
    hClose ficheiro --limpar ficheiro2

    finder anoMax (lines suporte2)
    repeater2 (anoMax-1)(salas-1)


imprimeSalas2 :: Int -> [String] -> IO()
imprimeSalas2 salas [] = return()
imprimeSalas2 0 (x:xs) = return()
imprimeSalas2 salas (x:xs) = do
    ficheiro <- openFile "tarefa2.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ " - "++ unwords (tail (tail(words x))))
    hClose ficheiro
    imprimeSalas2 (salas-1) xs


----------FUNÇÕES TAREFA 3----------

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
  | x `elem` xs = x : remDup (delete x xs)
  | otherwise = x : remDup xs

tarefa3_1 :: IO()
tarefa3_1 = do
    ficheiro <- openFile "Suporte1.txt" WriteMode
    hClose ficheiro
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"
    putStrLn "indique o nome da disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines ucs) (lines inscricoes) (lines alunos)
    putStrLn "indique o nome da outra disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines ucs) (lines inscricoes) (lines alunos)
    
encontrarNome:: String -> [String] -> [String] -> [String]-> IO() --descobre o numero da disciplina a partir do nome
encontrarNome x [] y z = return()
encontrarNome input (linha:linhas) conteudo_insc conteudo_alunos = do
    let numero = head (words linha)
    if input == unwords (tail(tail(words linha)))
        then descobrirAlxxx numero conteudo_insc conteudo_alunos --funcao usada na tarefa 1, com o numero descobre o al
        else return()
    encontrarNome input linhas conteudo_insc conteudo_alunos  

descobrirAlxxx:: String -> [String] -> [String]-> IO() --descobre o al a partir do numero da disciplina
descobrirAlxxx numero [] conteudo_alunos = return()
descobrirAlxxx numero (linha:linhas) conteudo_alunos = do
    
    let numero_al = head (words linha) -- al do aluno
    if last (words linha) == numero
        then do 
            ficheiro <- openFile "Suporte1.txt" AppendMode
            hPutStrLn ficheiro (head (words linha))--tranforma al em nome 
            hClose ficheiro
        else return ()
    descobrirAlxxx numero linhas conteudo_alunos    


----------FUNÇÕES TAREFA 4----------
escalonamento4 :: [String]-> [String]-> [String]->Int -> Int -> [String] -> IO() 
escalonamento4 disciplina inscricao alunos dias salas [] = return()
escalonamento4 disciplina inscricao alunos dias salas (x:xs) = do

    ficheiro <- openFile "Suporte1.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro

    imprimeSalas4 disciplina inscricao alunos salas (x:xs)

    ficheiroSuporte <- readFile "Suporte1.txt"
    let tamanhoInicial = length (lines ficheiroSuporte) 
    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Numero de imcompatibilidades: " ++ show incompativeis)
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
copyFile :: [String] -> IO()
copyFile [] = return()
copyFile (x:xs)= do 
    ficheiro <- openFile "Suporte2.txt" AppendMode
    hPutStrLn ficheiro x
    hClose ficheiro
    copyFile xs

escalonamento5 :: [String]-> [String]-> [String]->Int -> Int  -> IO() 
escalonamento5 disciplina inscricao alunos dias salas  = do

    
    ficheiro <- openFile "Suporte1.txt" WriteMode
    hClose ficheiro

    
    ficheiro <- openFile "tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro
   -- aqui tenho de fazer a selecao func select
    ficheiro <- openFile "Suporte3.txt" WriteMode
    hClose ficheiro
    
    repeater salas salas -- repete numero de salas ate encontrar a melhor combinacao que guarda no ficheiro 3

    suporte3 <- readFile' "Suporte3.txt"

    ficheiro <- openFile "Suporte1.txt" WriteMode
    hClose ficheiro

    imprimeSalas5 disciplina inscricao alunos salas (lines suporte3) --print selecao do suporte 3

    ficheiroSuporte <- readFile "Suporte1.txt"
    let tamanhoInicial = length (lines ficheiroSuporte) 
    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Numero de incompatibilidades: " ++ show incompativeis) -- print final das incompativilidad
    hClose ficheiro

    suporte2 <- readFile' "Suporte2.txt"
    
    if null (lines suporte2)
        then return()
        else escalonamento5 disciplina inscricao alunos (dias+1) salas

     
condTester :: Int -> Int-> [String] -> IO() 
condTester salas _ [] = return()
condTester 0 _ (x:xs) = return()-- tem de remover os valores utilizados do suporte2 e escolher os melhores para o suporte3  
condTester salas salasTotal (x:xs) = do -- numero de salas, ficheiro2
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"

    if salas == salasTotal
        then do 
            suporte3 <- openFile "Suporte3.txt" AppendMode
            hPutStrLn suporte3 x 
            hClose suporte3

            suporte2 <- openFile "Suporte2.txt" WriteMode
            hClose suporte2

            copyFile xs -- remove o x do suporte 2 pois ja foi escolhido
        else do 
            ficheiro <- openFile "Suporte1.txt" WriteMode
            hClose ficheiro --limpar o suporte 1 
            suporte3 <- readFile' "Suporte3.txt"
            let size = length (lines suporte3)
            loaderSup2 (lines suporte3) size --adicionar os valores de sup3 no 1 para testar
            -- load x
            let disciplinas = unwords(tail(tail(words x)))
            encontrarNome disciplinas (lines ucs) (lines inscricoes) (lines alunos)
            --testar
            ficheiroSuporte <- readFile' "Suporte1.txt"
            let tamanhoInicial = length (lines ficheiroSuporte) 
            let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
            let incompativeisx = tamanhoInicial - tamanhoFinal

            if null xs 
                then do 
                    suporte3 <- openFile "Suporte3.txt" AppendMode
                    hPutStrLn suporte3 x 
                    hClose suporte3

                    suporte2 <- openFile "Suporte2.txt" WriteMode
                    hClose suporte2
                    return()
                else do
                    --reload
                    ficheiro <- openFile "Suporte1.txt" WriteMode
                    hClose ficheiro --limpar o suporte 1 
                    suporte3 <- readFile' "Suporte3.txt"
                    let size = length (lines suporte3)
                    loaderSup2 (lines suporte3) size --adicionar os valores de sup3 no 1 para testar
                    --load xs
                    let disciplinas = unwords(tail(tail(words (head xs))))
                    encontrarNome disciplinas (lines ucs) (lines inscricoes) (lines alunos)
                    --testar
                    ficheiroSuporte <- readFile' "Suporte1.txt"
                    let tamanhoInicial = length (lines ficheiroSuporte) 
                    let tamanhoFinal = length (remDup(lines ficheiroSuporte)) 
                    let incompativeisxs = tamanhoInicial - tamanhoFinal

                    if incompativeisxs >= incompativeisx
                        then do 
                            suporte3 <- openFile "Suporte3.txt" AppendMode
                            hPutStrLn suporte3 x 
                            hClose suporte3

                            suporte2 <- openFile "Suporte2.txt" WriteMode
                            hClose suporte2

                            copyFile xs -- remove o x do suporte 2 pois ja foi escolhido

                        else do 
                            suporte3 <- openFile "Suporte3.txt" AppendMode
                            hPutStrLn suporte3 (head xs )
                            hClose suporte3

                            suporte2 <- openFile "Suporte2.txt" WriteMode
                            hPutStrLn suporte2 x
                            hClose suporte2
                            
                            copyFile (tail xs) -- remove o x do suporte 2 pois ja foi escolhido

repeater :: Int -> Int-> IO()
repeater 0 salasTotal = return()
repeater salas salasTotal = do 

    suporte2 <- readFile' "Suporte2.txt"
    condTester salas salasTotal (lines suporte2)

    repeater (salas-1) salasTotal


loaderSup2 :: [String] -> Int -> IO()
loaderSup2 [] size = return()
loaderSup2 (x:xs) 0 = return()
loaderSup2 (x:xs) size = do --disciplina e sup3 para comparar size sup3

    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    alunos <- readFile "listaalunos.txt"


    let disciplinas = unwords(tail(tail(words x)))
    encontrarNome disciplinas (lines ucs) (lines inscricoes) (lines alunos)

    loaderSup2 xs (size-1)


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

    
----------FUNÇÕES TAREFA 6----------

formatacao6 :: [String]->[String]->[String]->[String]->IO()
formatacao6 disciplina inscricao alunos [] = return()
formatacao6 disciplina inscricao alunos (x:xs) = do
    

    let disciplinas = unwords(tail(tail(words x)))

    --clear file 
    ficheiro2 <- openFile "Suporte1.txt" WriteMode
    hClose ficheiro2 

    encontrarNome disciplinas disciplina inscricao alunos

    suporte1 <- readFile "Suporte1.txt"
    let alunosInscritos = length (lines suporte1)

    suporte2 <- openFile "Suporte2.txt" AppendMode
    hPutStrLn suporte2 (show alunosInscritos++" "++disciplinas)
    hClose suporte2

    formatacao6 disciplina inscricao alunos xs

escalonamento6 :: Int -> Int -> Int -> [Int] -> [String] -> IO() 
escalonamento6 dias diasMax salas lotacao [] = return()
escalonamento6 dias diasMax salas lotacao (x:xs) = do
    if dias > diasMax 
        then return()
        else do
            ficheiro <- openFile "tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            imprimeSalas6 salas lotacao (x:xs) --x:xs é suporte2

            conteudoSuporte2 <- readFile' "Suporte2.txt"
            escalonamento6 (dias+1) diasMax salas lotacao (lines conteudoSuporte2)
  
imprimeSalas6 :: Int -> [Int] -> [String] -> IO()
imprimeSalas6 salas (y:ys) [] = return()
imprimeSalas6 0 (y:ys) (x:xs) = return()
imprimeSalas6 salas [] [] = return()
imprimeSalas6 salas [] (x:xs) = return()
imprimeSalas6 salas (y:ys) (x:xs) = do

    --lotacao atual é y da sala
    
     
    suporte <- readFile' "Suporte1.txt" -- numero de alunos na turma

    let disciplina = unwords (tail(words x))


    --numero de alunos na turma - lotacao da sala
    let alunosTurma = head(words suporte)
    let alunosTurmaint = read alunosTurma :: Int
    let alunosRestantes = alunosTurmaint - y 
    if alunosTurmaint > y
        then do 
            ficheiro <- openFile "tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Sala "++ show salas ++ " "++ show y ++"/"++show y ++": "++ disciplina)
            hClose ficheiro
        else do 
            ficheiro <- openFile "tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Sala "++ show salas ++ " "++ show alunosTurmaint ++"/"++show y ++": "++ disciplina)
            hClose ficheiro
    
    if alunosRestantes > 0 
        then do 
            ficheirosuporte <- openFile "Suporte1.txt" WriteMode
            hPutStrLn ficheirosuporte (show alunosRestantes ++ " " ++ disciplina)
            hClose ficheirosuporte
            imprimeSalas6 (salas-1) ys (x:xs)
            return()
        else do 
            if null xs 
                then do 
                ficheiro2 <- openFile "Suporte2.txt" WriteMode -- clear file
                hClose ficheiro2 
                escreverFicheiro xs
                imprimeSalas6 (salas-1) ys xs
                return()    
                else do 
                ficheirosuporte <- openFile "Suporte1.txt" WriteMode
                hPutStrLn ficheirosuporte (head xs)  --escrever primeira linha do suporte2 no suporte
                hClose ficheirosuporte
                ficheiro2 <- openFile "Suporte2.txt" WriteMode -- clear file
                hClose ficheiro2 
                escreverFicheiro xs
                imprimeSalas6 (salas-1) ys xs
                return()    

escreverFicheiro :: [String] -> IO()
escreverFicheiro [] = return()
escreverFicheiro (x:xs)= do 
            ficheiro2 <- openFile "Suporte2.txt" AppendMode
            hPutStrLn ficheiro2 x
            hClose ficheiro2 
            escreverFicheiro xs