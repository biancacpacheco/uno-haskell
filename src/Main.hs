--menu de start!!



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
        distribuiJogo (geraBaralho)
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


-- o jogo:

data Cor = Vermelho | Verde | Azul | Amarelo deriving (Eq, Show, Read, Enum)
data ValorColorido = Zero | Um | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | MaisDois | Inverte | Bloqueio deriving (Eq, Show, Read, Enum)
data ValorCoringa = MaisQuatro | TrocaCor deriving (Eq, Show, Read, Enum)
data Carta = CartaColorida {cor :: Cor, valor :: ValorColorido} | CartaCoringa {valorCoringa :: ValorCoringa} deriving (Eq, Show, Read)

geraBaralho :: [Carta]
geraBaralho = geraCartasColoridas ++ geraCartasCoringa

geraBaralho2 :: [Carta]
geraBaralho2 = CartaCoringa {valorCoringa = MaisQuatro} : CartaCoringa {valorCoringa = TrocaCor} : geraCartasColoridas

geraCartasColoridas :: [Carta]
geraCartasColoridas = [CartaColorida {cor = c, valor = v} | c <- [Vermelho .. Amarelo], v <- [Zero .. Bloqueio]]

geraCartasCoringa :: [Carta]
geraCartasCoringa = [CartaCoringa {valorCoringa = vc} | vc <- [MaisQuatro, TrocaCor]]

distribuiJogo :: [Carta] -> IO()
distribuiJogo baralho = pegamaos (baralho ++ baralho)

pegamaos :: [Carta] -> IO()
pegamaos baralho = do
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
comecaPartida jogo =  do
    if (gameOver jogo) then
        print "Fim de jogo :D"
    else jogada ((jogo !!0)!!0) (drop 1 (jogo !!0)) 1 False (drop 1 jogo) 

--jogada :: Int -> Carta -> [[Carta]] -> [[Carta]]
jogada :: Carta -> [Carta] -> Int -> Bool -> [[Carta]] -> IO()
jogada cartamesa bolo quemjoga inverteflag maos = do
  let jogador = maos !!0
  let bot1 = maos !!1
  let bot2 = maos !!2
  let bot3 = maos !!3
  
  print ("################### UNO #####################") 
  putStrLn("\n")
  print ("Carta na mesa: " ++ show(cartamesa))
  putStrLn("\n")
  print ("Jogador: " ++ show(jogador))
  putStrLn("\n")
  print ("Bot 1: [" ++ show(length(bot1))++"]")
  putStrLn("\n")
  print ("Bot 2: [" ++ show(length(bot2))++"]")
  putStrLn("\n")
  print ("Bot 3: [" ++ show(length(bot3))++"]")
  putStrLn("\n")
  if (bolo == [] || gameOver maos) then
    print("O jogaodor: " ++ show(quemjoga) ++ " ganhou!")
  else do 
  print ("em desenvolvimento")




localiza :: [a] -> Int -> a
localiza x y
    | y > length x - 1 = error "Posição excede o tamanho da lista"
    | otherwise = head(drop y x)
    
remove :: Int -> [Carta] -> [Carta]
remove 1 (a:x) = x
remove n (a:x) = a: remove (n-1) x

gameOver :: [[Carta]] -> Bool
gameOver jogo = [] `elem` jogo

ehCoringa :: Carta -> Bool
ehCoringa carta
    | carta == CartaCoringa {valorCoringa = MaisQuatro} = True
    | carta == CartaCoringa {valorCoringa = TrocaCor} = True
    | otherwise = False

proxjoga :: Int -> Bool -> Int
proxjoga x inverte
    | x == 1 && inverte == False = 2
    | x == 2 && inverte == False = 3
    | x == 3 && inverte == False = 4
    | x == 4 && inverte == False = 1
    | x == 1 && inverte == True = 4
    | x == 2 && inverte == True = 1
    | x == 3 && inverte == True = 2
    | x == 4 && inverte == True = 3
    | otherwise = 9

