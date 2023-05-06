import System.IO ()

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