module Tarefa1 where
import System.IO ()

main1 :: IO()
main1 = do
    ucs <- readFile "ucs.txt"
    inscricoes <- readFile "inscricoes.txt"
    procuraUCs1 (lines ucs) inscricoes
    putStrLn "====================\n"

procuraUCs1 :: [String] -> String -> IO ()
procuraUCs1 [] _ = putStrLn ""
procuraUCs1 (linha:linhas) inscricoes = do
                        putStrLn ("\n====================\n"++unwords(tail(tail(words linha)))++"\n--------------------")
                        procuraInscricoes1 (head(words linha)) (lines inscricoes)
                        procuraUCs1 linhas inscricoes

procuraInscricoes1 :: String -> [String] -> IO()
procuraInscricoes1 _ [] = putStrLn ""
procuraInscricoes1 uc (linha:linhas) = if uc == last(words linha) then do alunos <- readFile "listaalunos.txt"
                                                                          procuraAlunos1 (head(words linha)) (lines alunos)
                                                                          procuraInscricoes1 uc linhas
                                else procuraInscricoes1 uc linhas
                    

procuraAlunos1 :: String -> [String] -> IO()
procuraAlunos1 _ [] = putStrLn ""
procuraAlunos1 al (linha:linhas) = if al == head(words linha) then do putStrLn (unwords(tail(tail(words linha))))
                                    else procuraAlunos1 al linhas
