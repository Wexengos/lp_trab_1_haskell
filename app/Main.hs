
import System.Random (randomRIO)

-- Interface para o tipo do dado
data Dado = Dado { face :: Int } deriving (Show, Eq)

-- cria um novo dado com uma face random
novoDado :: IO Dado
novoDado = do
    faceSorteada <- randomRIO (1, 6)
    return $ Dado faceSorteada

-- rotaciona o dado
rotacionarDado :: Dado -> Dado
rotacionarDado (Dado face)
    | face == 1 = Dado 1 
    | otherwise = Dado (face - 1)

-- verifica face do dado caso = 1 remove
podeRemover :: Dado -> Bool
podeRemover (Dado face) = face == 1

-- verifica caso de um dado para condição de vitória
verificaUmDado :: Dado -> Bool
verificaUmDado (Dado face)
  | face `elem` [1, 3, 4, 6] = True
  | face `elem` [2,5] = True
  | otherwise = False
-- verifica caso de dois dados para condição de vitória
verificaDoisDados :: Dado -> Dado -> Bool
verificaDoisDados (Dado face1) (Dado face2)
    | face1 + face2 == 7 = False
    | face1 == face2 = False
    | otherwise = True

-- verifica caso de mais de dois dados para condição de vitória
verificaMultiplosDados :: [Dado] -> Bool
verificaMultiplosDados dados
   | any (\(Dado f) -> f == 2 || f == 5) dados = False
   | otherwise                 = not (all (\(Dado f1, Dado f2) -> f1 == f2 || f1 + f2 == 7) pares)
   where
    pares = [(dados !! i, dados !! j) | i <- [0..length dados - 1], j <- [i+1..length dados - 1]]

-- intacia de todas as função de casos para verificação de vitórias
verificarEstrategia :: [Dado] -> Bool
verificarEstrategia [dado] = verificaUmDado dado
verificarEstrategia [dado1, dado2] = verificaDoisDados dado1 dado2
verificarEstrategia dados = verificaMultiplosDados dados


-- função para jogar os dados
jogarDados :: Int -> Int -> IO ()
jogarDados numDados dificuldade = do
    dados <- sequence $ replicate numDados novoDado
    jogar dados dificuldade True

-- função principal do jogo
jogar :: [Dado] -> Int -> Bool -> IO ()
jogar [] _ turnoJogador = do if turnoJogador then putStrLn "Computador Ganhou!" else putStrLn "O Jogador Ganhou!"
jogar dados dificuldade turnoJogador = do
    putStrLn $ "Estado atual: " ++ show dados
    if turnoJogador
        then jogadaHumano dados dificuldade
        else jogadaComputador dados dificuldade

-- função para jogada do jogador
jogadaHumano :: [Dado] -> Int -> IO ()
jogadaHumano dados dificuldade = do
    putStrLn "Sua vez. Escolha um dado (1-n):"
    escolha <- readLn
    if escolha < 1 || escolha > length dados
        then do
            putStrLn "Escolha inválida. Tente novamente."
            jogadaHumano dados dificuldade
        else do
            let dadoEscolhido = dados !! (escolha - 1)
            if podeRemover dadoEscolhido
                then do
                    putStrLn "Dado removido!"
                    jogar (take (escolha - 1) dados ++ drop escolha dados) dificuldade False
                else do
                    putStrLn "Dado rotacionado."
                    jogar (take (escolha - 1) dados ++ [rotacionarDado dadoEscolhido] ++ drop escolha dados) dificuldade False

-- função para jogada do computador
jogadaComputador :: [Dado] -> Int -> IO ()
jogadaComputador dados dificuldade = do
    putStrLn "Vez do computador..."
    escolha <- randomRIO (0, length dados - 1)
    let dadoEscolhido = dados !! escolha
    if dificuldade == 1
        then if podeRemover dadoEscolhido
            then do
                putStrLn "Computador removeu um dado."
                jogar (take escolha dados ++ drop (escolha + 1) dados) dificuldade True
            else do
                putStrLn "Computador rotacionou um dado."
                jogar (take escolha dados ++ [rotacionarDado dadoEscolhido] ++ drop (escolha + 1) dados) dificuldade True
    else do 
        if verificarEstrategia dados
            then do
                putStrLn "Computador removeu um dado."
                jogar (take escolha dados ++ drop (escolha + 1) dados) dificuldade True
            else do
                putStrLn "Computador rotacionou um dado."
                jogar (take escolha dados ++ [rotacionarDado dadoEscolhido] ++ drop (escolha + 1) dados) dificuldade True
-- main
main :: IO ()
main = do
    putStrLn "Bem-vindo ao Jogo dos Dados!"
    putStrLn "Escolha a dificuldate (1) Fácil (2) Difícil"
    dificuldade <- readLn
    putStrLn "Quantos dados você quer usar?"
    numDados <- readLn
    jogarDados numDados dificuldade


-- executa cabal run Trab-lp