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
    else
      putStrLn "peraí"