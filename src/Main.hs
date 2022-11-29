--menu de start!!

import System.Random.Shuffle (shuffleM)


main:: IO()
main = do
    putStrLn("BEM-VINDO, JOGADOR! :)")
    putStrLn("")
    menuStart


menuStart :: IO()
menuStart = do
    putStrLn("escolha uma opção:")
    putStrLn("(1) iniciar partida")
    putStrLn("(2) histórico de pontuação")
    putStrLn("(3) regras do jogo")
    putStrLn("(4) sair do jogo")
    opcao <- getLine
    verificaOpcao (read opcao)


verificaOpcao :: Int -> IO()
verificaOpcao opcao =
    if opcao == 1 then
        menuJogo
    else if opcao == 2 then
        historico
    else if opcao == 3 then
        regrasJogo
    else if opcao == 4 then
        putStrLn("Até a próxima :)")
    else do         
        putStrLn("Por favor, escolha uma opção válida.") 
        menuStart


regrasJogo :: IO()
regrasJogo = do
    putStrLn "\n--------------------------- Regras do jogo --------------------------- \n"
    putStrLn "Objetivo do jogo: \nSeja o primeiro jogador a se livrar de todas as suas cartas.\n"
    putStrLn "Como jogar UNO: \nNa sua vez, você deve combinar uma carta da sua mão com aquela presente na pilha de Descarte. A carta jogada sempre deve ser da mesma cor ou do mesmo numero da carta presente no baralho, exceto quando uma carta curinga ou uma +4 tiverem sido jogadas.\n"
    putStrLn "Funções das cartas de ação: "
    putStrLn "Comprar duas cartas (+2): Quando esta carta for jogada, o próximo jogador deve comprar 2 cartas e perde a vez. Ela apenas pode ser jogada sobre uma cor que combine. \n"
    putStrLn "Comprar quatro cartas (+4): Ao jogar esta carta, você pode escolher a cor a ser jogada, além de fazer com que o próximo jogador tenha que comprar 4 cartas da pilha de Compras, perdendo também a vez. \n"
    putStrLn "Bloqueio: O próximo a jogar perde a vez. \n"
    putStrLn "Inverter: Ao descartar esta carta, o sentido do jogo é invertido (se estiver indo para a esquerda, muda para a direita e vice-versa). \n"
    putStrLn "Curinga: O jogador que lançou essa carta escolhe a nova cor que continuará no jogo. \n"
    putStrLn "----------------------------------------------------------------------"
    putStrLn "\nPressione `Enter` para retornar ao Menu Principal"
    
    a <- getLine
    menuStart


historico :: IO()
historico = do
    putStrLn"Histórico: \npressione Enter para retornar ao Menu Principal"
    a <- getLine
    menuStart


--menu uno
--precisa escolher quantos bots pra iniciar

menuJogo:: IO()
menuJogo = do
    putStrLn "Escolha a quantidade de jogadores:"
    putStrLn "(3) jogadores (você contra dois bots)"
    putStrLn "(4) jogadores (você contra três bots)"
    j <- getLine
    if ((read j) /= 3 && (read j) /= 4) then do
        putStrLn "Por favor, escolha uma opção válida. \n-----------------------------"
        menuJogo
    else distribuiJogo (read j) geraBaralho

-- o jogo:

data Cor = Vermelho | Verde | Azul | Amarelo deriving (Eq, Show, Read, Enum, Bounded)
data ValorColorido = Zero | Um | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | MaisDois | Inverte | Bloqueio deriving (Eq, Show, Read, Enum, Bounded)
data ValorCoringa = MaisQuatro | TrocaCor deriving (Eq, Show, Read, Enum, Bounded)
data Carta = CartaColorida {cor :: Cor, valor :: ValorColorido} | CartaCoringa {valorCoringa :: ValorCoringa} deriving (Eq, Show, Read, Enum, Bounded)

geraBaralho :: [Carta]
geraBaralho = geraCartasColoridas ++ geraCartasCoringa

geraBaralho2 :: [Carta]
geraBaralho2 = CartaCoringa {valorCoringa = MaisQuatro} : CartaCoringa {valorCoringa = TrocaCor} : geraCartasColoridas

geraCartasColoridas :: [Carta]
geraCartasColoridas = [CartaColorida {cor = c, valor = v} | c <- [Vermelho .. Amarelo], v <- [Zero .. Bloqueio]]

geraCartasCoringa :: [Carta]
geraCartasCoringa = [CartaCoringa {valorCoringa = vc} | vc <- [MaisQuatro, TrocaCor]]

distribuiJogo :: Int -> [Carta] -> IO()
distribuiJogo numJogadores baralho
  | numJogadores == 3 = pegamao3 baralho
  | numJogadores == 4 = pegamao4 baralho
  | otherwise         = print ""

pegamao3 :: [Carta] -> IO()
pegamao3 baralho = do
   let mao1 = take 7 baralho
   let brl1 = drop 7 baralho
   let mao2 = take 7 brl1
   let brl2 = drop 7 brl1
   let mao3 = take 7 brl2
   let brl3 = drop 7 brl2
   comecaPartida [brl3, mao1, mao2, mao3]

pegamao4 :: [Carta] -> IO()
pegamao4 baralho = do
   let mao1 = take 7 baralho
   let brl1 = drop 7 baralho
   let mao2 = take 7 brl1
   let brl2 = drop 7 brl1
   let mao3 = take 7 brl2
   let brl3 = drop 7 brl2
   let mao4 = take 7 brl3
   let brl4 = drop 7 brl3
   comecaPartida [brl4, mao1, mao2, mao3, mao4]
   
comecaPartida :: [[Carta]] -> IO()
comecaPartida jogo = 
    
    if (gameOver jogo)
        print "Fim de jogo :D"
    else
        jogada 

jogada :: Int -> Carta -> [[Carta]] -> [[Carta]]

-- saber quem eh o jogador, jogo, mesa

[0] = baralho
[1] = j1
[2] = j2
[3] = j3

gameOver :: [[Carta]] -> Bool
gameOver jogo = elem `[]` jogo

ehCoringa :: Carta -> Bool
ehCoringa carta
    | carta == CartaCoringa {valorCoringa = MaisQuatro} = True
    | carta == CartaCoringa {valorCoringa = TrocaCor} = True
    | otherwise = False


-- um dos jogadores nao ter carta ou o baralho ser uma lista vazia