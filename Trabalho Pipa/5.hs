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

-- Função que filtra as inscrições para obter apenas aquelas referentes a uma determinada UC
inscricoesPorUC :: UC -> [Inscricao] -> [Aluno]
inscricoesPorUC uc inscricoes = nub [aluno | (uc', aluno) <- inscricoes, uc' == uc]

-- Função que filtra as inscrições para obter apenas aquelas referentes a um determinado aluno
inscricoesPorAluno :: Aluno -> [Inscricao] -> [UC]
inscricoesPorAluno aluno inscricoes = nub [uc | (uc, aluno') <- inscricoes, aluno' == aluno]

-- Função que imprime na tela os alunos inscritos em uma determinada UC
imprimeAlunos :: UC -> [UC] -> [Aluno] -> [Inscricao] -> IO ()
imprimeAlunos uc ucs alunos inscricoes = do
  let alunosUC = inscricoesPorUC uc inscricoes
  putStrLn ("Alunos inscritos em " ++ uc ++ ":")
  mapM_ putStrLn alunosUC
  putStrLn ""

-- Função que imprime na tela as UCs às quais um determinado aluno está inscrito
imprimeUCs :: Aluno -> [UC] -> [Inscricao] -> IO ()
imprimeUCs aluno ucs inscricoes = do
  let ucsAluno = inscricoesPorAluno aluno inscricoes
  putStrLn ("UCs às quais " ++ aluno ++ " está inscrito:")
  mapM_ putStrLn ucsAluno
  putStrLn ""

-- Função que apresenta um menu com as possibilidades de listagem e lê a escolha do utilizador
menu :: [UC] -> [Aluno] -> [Inscricao] -> IO ()
menu ucs alunos inscricoes = do
  putStrLn "Selecione uma das"