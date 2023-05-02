module Tarefa1 where
import System.IO
import Data.Graph (path)

type UC = String
type Aluno = String
type Inscricao = (UC, Aluno)

lerInscricoes :: IO[Inscricao]
lerInscricoes = do
    conteudo <- readFile "inscricoes.txt"
    let inscricoes =  map (fInscricao . words) (lines conteudo)
    return inscricoes
    where 
        fInscricao [uc, aluno] = (uc, aluno)

lerUCs :: IO[UC]
lerUCs = do
    conteudo <- readFile "ucs.txt"
    let ucs = lines conteudo
    return ucs
    
lerAlunos :: IO[Aluno]
lerAlunos = do
    conteudo <- readFile "listaalunos.txt"
    let alunos = lines conteudo
    return alunos

inscricoesPorUC :: UC -> [Inscricao] -> [Aluno]
inscricoesPorUC uc inscricoes = [aluno | (uc', aluno) <- inscricoes, uc' == uc]

imprimeInscricoes :: [UC] -> [Aluno] -> [Inscricao] -> IO ()
imprimeInscricoes ucs alunos inscricoes = mapM_ imprimeUC ucs
  where
    imprimeUC uc = do
      let alunosUC = inscricoesPorUC uc inscricoes
      putStrLn uc
      mapM_ putStrLn alunosUC
      putStrLn ""

main :: IO ()
main = do
    ucs <- lerUCs
    alunos <- lerAlunos
    inscricoes <- lerInscricoes
    imprimeInscricoes ucs alunos inscricoes






