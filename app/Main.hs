import System.Random (randomRIO)

-- Tipo para representar um dado
data Dado = Dado { face :: Int } deriving (Show, Eq)

-- Função para criar um novo dado com face aleatória
novoDado :: IO Dado
novoDado = do
    faceSorteada <- randomRIO (1, 6)
    return $ Dado faceSorteada

-- Função para rotacionar um dado
rotacionarDado :: Dado -> Dado
rotacionarDado (Dado face)
    | face == 1 = Dado 1  -- Não pode rotacionar se já estiver em 1
    | otherwise = Dado (face - 1)

-- Função para verificar se um dado pode ser removido
podeRemover :: Dado -> Bool
podeRemover (Dado face) = face == 1

-- Função principal do jogo
jogarDados :: Int -> IO ()
jogarDados numDados = do
    dados <- sequence $ replicate numDados novoDado
    jogar dados True

-- Loop principal do jogo
jogar :: [Dado] -> Bool -> IO ()
jogar [] _ = putStrLn "Fim do jogo!"
jogar dados turnoJogador = do
    putStrLn $ "Estado atual: " ++ show dados
    if turnoJogador
        then jogadaHumano dados
        else jogadaComputador dados

-- Implementação da jogada do jogador humano
jogadaHumano :: [Dado] -> IO ()
jogadaHumano dados = do
    putStrLn "Sua vez. Escolha um dado (1-n):"
    escolha <- readLn
    if escolha < 1 || escolha > length dados
        then do
            putStrLn "Escolha inválida. Tente novamente."
            jogadaHumano dados
        else do
            let dadoEscolhido = dados !! (escolha - 1)
            if podeRemover dadoEscolhido
                then do
                    putStrLn "Dado removido!"
                    jogar (take (escolha - 1) dados ++ drop escolha dados) False
                else do
                    putStrLn "Dado rotacionado."
                    jogar (take (escolha - 1) dados ++ [rotacionarDado dadoEscolhido] ++ drop escolha dados) False

-- Implementação simples da jogada do computador (nível fácil)
jogadaComputador :: [Dado] -> IO ()
jogadaComputador dados = do
    putStrLn "Vez do computador..."
    escolha <- randomRIO (0, length dados - 1)
    let dadoEscolhido = dados !! escolha
    if podeRemover dadoEscolhido
        then do
            putStrLn "Computador removeu um dado."
            jogar (take escolha dados ++ drop (escolha + 1) dados) True
        else do
            putStrLn "Computador rotacionou um dado."
            jogar (take escolha dados ++ [rotacionarDado dadoEscolhido] ++ drop (escolha + 1) dados) True

-- Função principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao Jogo dos Dados!"
    putStrLn "Quantos dados você quer usar?"
    numDados <- readLn
    jogarDados numDados


    