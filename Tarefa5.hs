module Tarefa5 where
import System.IO

--Função principal, usada para mostar ao utilizador uma interface do menu
main :: IO ()
main = do
    putStr "================MENU================\n|1| - ALUNOS INSCRITOS POR UC\n|2| - UCs INSCRITAS POR ALUNO\n|3| - PROCURA POR UC\n|4| - PROCURA POR ALUNO\n0 - SAIR\n=====================================\n\n-> "
    opcao <- getLine
    escolheOpcao opcao
    
--Função do menu    
escolheOpcao :: String -> IO ()
escolheOpcao opcao
            | opcao == "1" = do
                            imptodasuc
                            main
            | opcao == "2" = do
                            imptodosaluno
                            main
            | opcao == "3" = do
                            imprimeuc
                            main
            | opcao == "4" = do
                            imprimealuno
                            main
            | opcao == "0" = return()
            | otherwise = do
                            putStrLn "OPÇÃO INVÁLIDA"
                            main

imptodasuc :: IO()
imptodasuc = do
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    percucs (lines ucs) inscricoes

percucs :: [String] -> String -> IO()
percucs [] _ = putStrLn ("")
percucs (linha:linhas) inscricoes = do
                        putStrLn (unwords(tail(tail(words linha))))
                        percinsc (head(words linha)) (lines inscricoes)
                        percucs linhas inscricoes

percinsc :: String -> [String] -> IO()
percinsc _ [] = putStrLn ("")
percinsc uc (linha:linhas) = if uc == last(words linha) then do alunos <- readFile "listaalunos.txt"
                                                                percalunos (head(words linha)) (lines alunos)
                                                                percinsc uc linhas
                                else percinsc uc linhas

percalunos :: String -> [String] -> IO()
percalunos _ [] = putStrLn ("")
percalunos al (linha:linhas) = if al == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
                                    else percalunos al linhas

imptodosaluno :: IO()
imptodosaluno = do
    alunos <- readFile "listaalunos.txt"
    inscricoes <- readFile "inscricoes.txt"
    percorrealunos (lines alunos) inscricoes

percorrealunos :: [String] -> String -> IO()
percorrealunos [] _ = putStrLn ""
percorrealunos (linha:linhas) inscricoes = do
                            putStrLn (unwords(tail(tail(words linha))))
                            percorreinsc (head(words linha)) (lines inscricoes)
                            percorrealunos linhas inscricoes

percorreinsc :: String -> [String] -> IO()
percorreinsc _ [] = putStrLn ""
percorreinsc al (linha:linhas) = if al == head(words linha) then do ucs <- readFile "ucs.txt"
                                                                    percorreuc (last(words linha)) (lines ucs)
                                                                    percorreinsc al linhas
                                else percorreinsc al linhas

percorreuc :: String -> [String] -> IO()
percorreuc _ [] = putStrLn ""
percorreuc uc (linha:linhas) = if uc == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
                                else percorreuc uc linhas

imprimeuc :: IO()
imprimeuc = do
    putStrLn ("Escolha a UC")
    putStrLn ("1-Programacao Funcional")
    putStrLn ("2-Compiladores")
    putStrLn ("3-Topicos")
    putStrLn ("4-Fisica")
    uc <- getLine
    inscricoes <- readFile "inscricoes.txt"
    percinsc uc (lines inscricoes)

imprimealuno :: IO()
imprimealuno = do
    inscricoes <- readFile "inscricoes.txt"
    putStrLn "Escolha o aluno"
    putStrLn "1-Rui Silva"
    putStrLn "2-Maria Silva"
    putStrLn "3-Tiago Silva"
    putStrLn "4-Sofia Silva"
    aluno <- getLine
    if aluno == "1" then do 
        let al = "al001"
        percorreinsc al (lines inscricoes)
    else if aluno == "2" then do
        let al = "al002"
        percorreinsc al (lines inscricoes)
    else if aluno == "3" then do
        let al = "al003"
        percorreinsc al (lines inscricoes)
    else if aluno == "4" then do
        let al = "al004"
        percorreinsc al (lines inscricoes)
    else putStrLn "Erro"