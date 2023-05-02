import System.IO
import Data.List (nub)

type UC = String
type Aluno = String
type Inscricao = (UC, Aluno)

-- Função que lê as inscrições do arquivo "inscricoes.txt" e retorna uma lista de tuplas (UC, Aluno)
leInscricoes :: FilePath -> IO [Inscricao]
leInscricoes path = do
  conteudo <- readFile path
  let inscricoes = map (paraInscricao . words) (lines conteudo)
  return inscricoes
  where
    paraInscricao [uc, aluno] = (uc, aluno)
    paraInscricao _ = error "Formato inválido de inscrição"

-- Função que lê os nomes das UCs do arquivo "ucs.txt" e retorna uma lista de UCs
leUCs :: FilePath -> IO [UC]
leUCs path = do
  conteudo <- readFile path
  let ucs = lines conteudo
  return ucs

-- Função que lê os nomes dos alunos do arquivo "listaalunos.txt" e retorna uma lista de alunos
leAlunos :: FilePath -> IO [Aluno]
leAlunos path = do
  conteudo <- readFile path
  let alunos = lines conteudo
  return alunos

-- Função que filtra as inscrições para obter apenas aquelas referentes a um determinado aluno
inscricoesPorAluno :: Aluno -> [Inscricao] -> [UC]
inscricoesPorAluno aluno inscricoes = nub [uc | (uc, aluno') <- inscricoes, aluno' == aluno]

-- Função que imprime na tela as UCs às quais cada aluno está inscrito
imprimeInscricoes :: [Aluno] -> [UC] -> [Inscricao] -> IO ()
imprimeInscricoes alunos ucs inscricoes = mapM_ imprimeAluno alunos
  where
    imprimeAluno aluno = do
      let ucsAluno = inscricoesPorAluno aluno inscricoes
      putStrLn aluno
      mapM_ putStrLn ucsAluno
      putStrLn ""

-- Função principal que lê os arquivos e imprime as inscrições
main :: IO ()
main = do
  ucs <- leUCs "ucs.txt"
  alunos <- leAlunos "listaalunos.txt"
  inscricoes <- leInscricoes "inscricoes.txt"
  imprimeInscricoes alunos ucs inscricoes