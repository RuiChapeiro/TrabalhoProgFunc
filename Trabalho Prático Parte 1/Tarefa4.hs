module Tarefa4 where
import System.IO ()
import Tarefa2

main4 :: IO()
main4 = do inscricoes <- readFile "inscricoes.txt"
           putStr "\n==================================================\nQUAL DOS ALUNOS PRETENDE VISUALIZAR AS INSCRIÇÕES?\n==================================================\n1 - Rui Silva (al0001)\n2 - Maria Silva (al0002)\n3 - Tiago Silva (al0003)\n4 - Sofia Silva (al0004)\n->"
           aluno <- getLine
           putStrLn "--------------------"
           if aluno == "1" then do 
                let al = "al001"
                Tarefa2.procuraInscricoes2 al (lines inscricoes)
           else if aluno == "2" then do
                let al = "al002"
                Tarefa2.procuraInscricoes2 al (lines inscricoes)
           else if aluno == "3" then do
                let al = "al003"
                Tarefa2.procuraInscricoes2 al (lines inscricoes)
           else if aluno == "4" then do
                let al = "al004"
                Tarefa2.procuraInscricoes2 al (lines inscricoes)
           else putStrLn "Aluno Inválido"
           putStrLn "--------------------"