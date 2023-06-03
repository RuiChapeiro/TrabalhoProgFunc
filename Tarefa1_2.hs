module Tarefa1_2 where
import System.IO ()

main2 :: IO()
main2 = do alunos <- readFile "listaalunos.txt"
           inscricoes <- readFile "inscricoes.txt"
           procuraAlunos2 (lines alunos) inscricoes
           putStrLn "====================\n"

procuraAlunos2 :: [String] -> String -> IO()
procuraAlunos2 [] _ = putStrLn ""
procuraAlunos2 (linha:linhas) inscricoes = do 
    if null (words linha) then putStrLn ""
    else do putStrLn ("\n====================\n"++unwords(tail(tail(words linha)))++"\n--------------------")
            procuraInscricoes2 (head(words linha)) (lines inscricoes)
            procuraAlunos2 linhas inscricoes

procuraInscricoes2 :: String -> [String] -> IO()
procuraInscricoes2 _ [] = putStrLn ""
procuraInscricoes2 al (linha:linhas) = do
    if null (words linha) then putStrLn ""
    else do if al == head(words linha) then do
               ucs <- readFile "ucs.txt"
               procuraUCs2 (last(words linha)) (lines ucs)
               procuraInscricoes2 al linhas
            else procuraInscricoes2 al linhas

procuraUCs2 :: String -> [String] -> IO()
procuraUCs2 _ [] = putStrLn ""
procuraUCs2 uc (linha:linhas) = do
    if null (words linha) then putStrLn ""
    else do 
        if uc == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
        else procuraUCs2 uc linhas