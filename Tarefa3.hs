module Tarefa3 where
import System.IO ()
import qualified Tarefa1

main3 :: IO()
main3 = do putStr "\n==================================================\nQUAL DAS UC'S PRETENDE VISUALIZAR AS INSCRIÇÕES?\n==================================================\n1 - Programacao Funcional\n2 - Compiladores\n3 - Topicos\n4 - Fisica\n->"
           uc <- getLine
           putStrLn "--------------------"
           inscricoes <- readFile "inscricoes.txt"
           Tarefa1.procuraInscricoes1 uc (lines inscricoes)
           putStrLn "--------------------"