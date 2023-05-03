module Teste1 where

import System.IO
import Data.List (intersperse)

type UC = String
type Aluno = String
type Inscricao = String
type Dados = [(UC, [Aluno])]

lerInscricoes :: IO [Inscricao]
lerInscricoes = do
    conteudo <- readFile "inscricoes.txt"
    let inscricoes = lines conteudo
    return inscricoes

lerUCs :: IO [UC]
lerUCs = do
    conteudo <- readFile "ucs.txt"
    let ucs = lines conteudo
    return ucs

lerAlunos :: IO [Aluno]
lerAlunos = do
    conteudo <- readFile "listaAlunos.txt"
    let alunos = lines conteudo
    return alunos

lerDados :: IO Dados
lerDados = do
    ucs <- lerUCs
    let numUcs = length ucs
    inscricoes <- lerInscricoes
    let numInscricoes = length inscricoes
    alunos <- lerAlunos
    let numAlunos = length alunos
    let tuplas = map words inscricoes
    let inscricoes' = [(a,b) | [a,b] <- tuplas]
    let alunos' = [(a,b) | [a,b,_] <- map words alunos]
    let ucs' = [(a, []) | a <- ucs]
    let dados = foldr atualizar ucs' inscricoes'
    return dados

atualizar :: (Inscricao, Aluno) -> Dados -> Dados
atualizar (inscricao, aluno) dados = map (atualizarUC inscricao aluno) dados

atualizarUC :: Inscricao -> Aluno -> (UC, [Aluno]) -> (UC, [Aluno])
atualizarUC inscricao aluno (uc, alunos)
    | inscricao == uc = (uc, aluno:alunos)
    | otherwise = (uc, alunos)

imprimirDados :: Dados -> IO ()
imprimirDados dados = mapM_ imprimirUC dados

imprimirUC :: (UC, [Aluno]) -> IO ()
imprimirUC (uc, alunos) = do
    putStr uc
    putStr ": "
    putStrLn (concat (intersperse ", " alunos))

main :: IO ()
main = do
    dados <- lerDados
    imprimirDados dados