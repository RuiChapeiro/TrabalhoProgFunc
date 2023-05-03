module Tarefa1 where
import System.IO

type UC = String
type Aluno = String
type Inscricao = String

lerInscricoes :: IO[Inscricao]
lerInscricoes = do
    conteudo <- readFile "inscricoes.txt"
    let inscricoes =  lines conteudo
    return inscricoes

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

inscricoesPorUC :: UC -> [Aluno] -> [Inscricoes]
inscricoesPorUC ucs alunos inscricoes = do
    if last (words inscricoes) == head (words uc)
        then let last (inscricoes) = last (words aluno)

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
    inscricoesPorUC ucs alunos inscricoes