module Tarefa2 where
import System.IO
import qualified System.Posix.Internals as ucs



lerAlunos :: IO [Aluno]
lerAlunos = do 
    conteudo <- readFile "incricoes.txt"
    let linhas = lines conteudo 
    return alunos 

lerUCs :: IO [(String, String)]
lerUCs = do 
    conteudo <- readFile "ucs.txt"
    let linhas = lines conteudo
    
lerListaAlunos :: IO [(String,String)]
lerListaAlunos = do 
    conteudo <- readFile "listaalunos.txt"
    let linhas = lines conteudo




