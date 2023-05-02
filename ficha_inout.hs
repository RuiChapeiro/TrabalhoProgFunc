import System.IO


lenome :: IO()
lenome = do
    putStrLn "Qual é o teu nome?"
    nome <- getLine
    putStrLn ("Olá " ++ nome ++ ", como estás?")

imprimefich :: IO()
imprimefich = do
    conteudo <- readFile "nomes.txt"
    putStrLn conteudo

imprimenome :: IO()
imprimenome = do
    putStrLn "Qual é o teu nome?"
    nome <- getLine
    conteudo <- readFile "nomes.txt"
    encontranome nome (lines conteudo)

encontranome :: String -> [String] -> IO()
encontranome nome [] = putStrLn ("O nome " ++ nome ++ " não foi encontrado")
encontranome nome (linha:linhas) = do
                        if head (words linha) == nome 
                            then putStrLn linha 
                            else encontranome nome linhas

imprimeultimonome :: IO()
imprimeultimonome = do
    putStrLn "Qual é o teu nome?"
    nome <- getLine
    conteudo <- readFile "nomes.txt"
    encontraultimonome nome (lines conteudo)

encontraultimonome :: String -> [String] -> IO()
encontraultimonome nome [] = putStrLn ("O nome " ++ nome ++ " não foi encontrado")
encontraultimonome nome (linha:linhas) = do
                        if head (words linha) == nome 
                            then putStrLn (last (words linha))
                            else encontraultimonome nome linhas

imprimesegundonome :: IO()
imprimesegundonome = do
    putStrLn "Qual é o teu nome?"
    nome <- getLine
    conteudo <- readFile "nomes.txt"
    encontrasegundonome nome (lines conteudo)

encontrasegundonome :: String -> [String] -> IO()
encontrasegundonome nome [] = putStrLn ("O nome " ++ nome ++ " não foi encontrado")
encontrasegundonome nome (linha:linhas) = do
                        if head (words linha) == nome 
                            then putStrLn (words linha !! 1)
                            else encontrasegundonome nome linhas

gravanomesreversos :: IO()
gravanomesreversos = do
            conteudo <- readFile "nomes.txt"
            writeFile "nomesreversos.txt" "Nomes:\n"
            gravanomes (lines conteudo)

gravanomes :: [String] -> IO()
gravanomes [] = return ()
gravanomes (nome:nomes) = do
                appendFile "nomesreversos.txt" (last (words nome) ++ ", " ++ head (words nome) ++ "\n")
                gravanomes nomes


main :: IO ()
main = do
    putStrLn "1 - Imprime ficheiro \n2 - Imprime Nome\n3 - Grava nomes Reversos\n0 - Sair"
    opcao <- getLine
    lancaopcao opcao
    
    
lancaopcao :: String -> IO ()
lancaopcao opcao
            | opcao == "1" = do
                            imprimefich
                            main
            | opcao == "2" = do
                            imprimenome
                            main
            | opcao == "3" = do
                            gravanomesreversos
                            main
            | opcao == "0" = return()
            | otherwise = do
                            putStrLn "Opção não válida"
                            main

