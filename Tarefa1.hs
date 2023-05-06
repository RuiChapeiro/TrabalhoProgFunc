module Tarefa1 where
import System.IO ()

mainUM :: IO()
mainUM = do
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    procuraUCsUM (lines ucs) inscricoes
    putStrLn "====================\n"

procuraUCsUM :: [String] -> String -> IO ()
procuraUCsUM [] _ = putStrLn ""
procuraUCsUM (linha:linhas) inscricoes = do
                        putStrLn ("\n====================\n"++unwords(tail(tail(words linha)))++"\n--------------------")
                        procuraInscricoesUM (head(words linha)) (lines inscricoes)
                        procuraUCsUM linhas inscricoes

procuraInscricoesUM :: String -> [String] -> IO()
procuraInscricoesUM _ [] = putStrLn ""
procuraInscricoesUM uc (linha:linhas) = if uc == last(words linha) then do alunos <- readFile "listaalunos.txt"
                                                                           procuraAlunosUM (head(words linha)) (lines alunos)
                                                                           procuraInscricoesUM uc linhas
                                else procuraInscricoesUM uc linhas
                    

procuraAlunosUM :: String -> [String] -> IO()
procuraAlunosUM _ [] = putStrLn ""
procuraAlunosUM al (linha:linhas) = if al == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
                                    else procuraAlunosUM al linhas
