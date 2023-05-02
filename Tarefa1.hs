module Tarefa1 where
import System.IO
import Data.Graph (path)

type UC = String
type Aluno = String
type Inscricao = (UC, Aluno)

lerInscricoes :: IO[String]
lerInscricoes = do
    conteudo <- readFile "inscricoes.txt"
    let inscricoes = lines conteudo
    return inscricoes
    






