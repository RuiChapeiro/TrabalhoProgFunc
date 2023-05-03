module Tarefa2 where

import System.IO 

type Aluno = String
type UC = String

lerAlunos :: IO [Aluno]
lerAlunos = do
    conteudo <- readFile "inscricoes.txt"
    let linhas = lines conteudo
    return linhas

lerUCs :: IO [(UC, Aluno)]
lerUCs = do
    conteudo <- readFile "ucs.txt"
    let linhas = lines conteudo
        tuplas = [ (uc, aluno) | l <- linhas, let (uc:aluno:_) = words l ]
    return tuplas


relacionarAlunosUCs :: [Aluno] -> [(UC, Aluno)] -> [(Aluno, [UC])]
relacionarAlunosUCs alunos ucs = [(aluno, [uc | (uc, a) <- ucs, a == aluno]) | aluno <- alunos]

lerListaAlunos :: IO [(Aluno, [UC])]
lerListaAlunos = do
    alunos <- lerAlunos
    ucs <- lerUCs
    let lista = relacionarAlunosUCs alunos ucs
    return lista

mostrarListaAlunos :: [(Aluno, [UC])] -> IO ()
mostrarListaAlunos [] = return ()
mostrarListaAlunos ((aluno, ucs):resto) = do
    putStr "Aluno: "
    putStrLn aluno
    putStr "Unidades Curriculares: "
    putStrLn (unwords ucs)
    mostrarListaAlunos resto


main :: IO ()
main = do
    lista <- lerListaAlunos
    mostrarListaAlunos lista


