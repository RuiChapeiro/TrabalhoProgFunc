import System.IO

-- Função para ler um arquivo de texto e retornar seu conteúdo como uma lista de strings
readFileLines :: FilePath -> IO [String]
readFileLines filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let linesOfFile = lines contents
  hClose handle
  return linesOfFile

-- Tipo que representa uma unidade curricular
data UC = UC { ucCodigo :: String, ucNome :: String } deriving Show

-- Tipo que representa uma inscrição de um aluno em uma unidade curricular
data Inscricao = Inscricao { inscricaoAluno :: String, inscricaoUC :: String } deriving Show

-- Tipo que representa um aluno
data Aluno = Aluno { alunoCodigo :: String, alunoNome :: String } deriving Show

-- Função para converter uma linha do arquivo de unidades curriculares para um valor do tipo UC
parseUCLinha :: String -> UC
parseUCLinha linha = UC codigo nome
  where
    (codigo, restante) = break (== ';') linha
    nome = tail restante

-- Função para converter uma linha do arquivo de inscrições para um valor do tipo Inscricao
parseInscricaoLinha :: String -> Inscricao
parseInscricaoLinha linha = Inscricao aluno uc
  where
    (aluno, restante) = break (== ';') linha
    uc = tail restante

-- Função para converter uma linha do arquivo de alunos para um valor do tipo Aluno
parseAlunoLinha :: String -> Aluno
parseAlunoLinha linha = Aluno codigo nome
  where
    (codigo, restante) = break (== ';') linha
    nome = tail restante

-- Função para obter uma lista de todas as unidades curriculares a partir do arquivo ucs.txt
getUCs :: FilePath -> IO [UC]
getUCs filePath = do
  linhas <- readFileLines filePath
  let ucs = map parseUCLinha linhas
  return ucs

-- Função para obter uma lista de todas as inscrições a partir do arquivo inscricoes.txt
getInscricoes :: FilePath -> IO [Inscricao]
getInscricoes filePath = do
  linhas <- readFileLines filePath
  let inscricoes = map parseInscricaoLinha linhas
  return inscricoes

-- Função para obter uma lista de todos os alunos a partir do arquivo listaalunos.txt
getAlunos :: FilePath -> IO [Aluno]
getAlunos filePath = do
  linhas <- readFileLines filePath
  let alunos = map parseAlunoLinha linhas
  return alunos

-- Função para obter uma lista de todas as unidades curriculares de um determinado aluno
getUCsPorAluno :: String -> [Inscricao] -> [UC] -> [UC]
getUCsPorAluno codigoAluno inscricoes ucs = getUCsAux codigoAluno inscricoes ucs []
  where
    getUCsAux _ [] _ ucsAluno = ucsAluno
    getUCsAux codigoAluno (i:is) ucs ucsAluno
      | codigoAluno == inscricaoAluno i = getUCsAux codigoAluno is ucs (ucsAluno ++ [ucPorCodigo (
