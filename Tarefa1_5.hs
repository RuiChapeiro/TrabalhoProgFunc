module Tarefa1_5 where
import System.IO
import Tarefa1_1
import Tarefa1_2
import Tarefa1_3
import Tarefa1_4

--Função principal, usada para mostar ao utilizador uma interface do menu
menu :: IO ()
menu = do
    putStr "\n================MENU================\n|1| - ALUNOS INSCRITOS POR UC\n|2| - UCs INSCRITAS POR ALUNO\n|3| - PROCURA POR UC\n|4| - PROCURA POR ALUNO\n|0| - SAIR\n=====================================\n\n-> "
    opcao <- getLine
    escolheOpcao opcao
    
--Função do menu    
escolheOpcao :: String -> IO ()
escolheOpcao opcao
            | opcao == "1" = do
                            Tarefa1_1.main1
                            putStr "PRESSIONE QUALQUER TECLA PARA CONTINUAR..."
                            _ <- getLine
                            menu
            | opcao == "2" = do
                            Tarefa1_2.main2
                            putStr "PRESSIONE QUALQUER TECLA PARA CONTINUAR..."
                            _ <- getLine
                            menu
            | opcao == "3" = do
                            Tarefa1_3.main3
                            putStr "PRESSIONE QUALQUER TECLA PARA CONTINUAR..."
                            _ <- getLine
                            menu
            | opcao == "4" = do
                            Tarefa1_4.main4
                            putStr "PRESSIONE QUALQUER TECLA PARA CONTINUAR..."
                            _ <- getLine
                            menu
            | opcao == "0" = return()
            | otherwise = do
                            putStrLn "OPÇÃO INVÁLIDA\n\nPRESSIONE QUALQUER TECLA PARA CONTINUAR..."
                            _ <- getLine
                            menu

