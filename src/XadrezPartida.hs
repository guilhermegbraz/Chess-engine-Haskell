module XadrezPartida where

import XadrezElementos

iniciaJogo :: Jogo
iniciaJogo = Jogo Branco tabuleiroInicial PrimeiroMovimento


tabuleiroInicial :: Tabuleiro
tabuleiroInicial = [
        [Ocupada ((map (criaPecas Preto) [Torre, Cavalo, Bispo, Rainha, Rei, Bispo, Cavalo, Torre]) !! x ) (x,7)| x <- [0..7]]
        ,[Ocupada (Peca Peao Preto) (x,6) | x <- [0..7]]
    ] 
    ++
    reverse [[Empty (x,y) | x <- [0..7]] |  y <- [2..5]]
    ++
    [
        [Ocupada (Peca Peao Branco) (x,1) | x <- [0..7]]
        ,[Ocupada ((map (criaPecas Branco) [Torre, Cavalo, Bispo, Rainha, Rei, Bispo, Cavalo, Torre]) !! x ) (x,0)| x <- [0..7]]
    ]
    
    where
        criaPecas :: Cor -> TipoPeca -> Peca
        criaPecas cor tipo = Peca tipo cor


jogada :: Jogo -> Posicao -> Posicao -> Jogo
jogada jogo posI posF =
    if retornoMovValido 
        then verificaFimOuCheck novaPosicao 
        else Jogo (getTurno jogo) (getTabuleiro jogo) retornoResultado
    where
        (Retorno retornoMovValido retornoResultado) = isMovimentoValido jogo posI posF
        novaPosicao = Jogo (trocarTurno (getTurno jogo)) (realizarMovimento jogo posI posF) retornoResultado


jogoTerminou:: Jogo -> Bool
jogoTerminou jogo = isReiAfogado jogo || isCheckMate jogo || isEmpate jogo

verificaFimOuCheck :: Jogo -> Jogo
verificaFimOuCheck jogo = 
    if jogoTerminou jogo 
        then 
            if isCheckMate jogo 
                then Jogo (getTurno jogo) (getTabuleiro jogo) CheckMate
                else if isReiAfogado jogo 
                    then Jogo (getTurno jogo) (getTabuleiro jogo) ReiAfogado 
                    else Jogo (getTurno jogo) (getTabuleiro jogo) Empate

        else if isReiInCheck jogo (getTurno jogo)
            then Jogo (getTurno jogo) (getTabuleiro jogo) ReiEmCheck
            else jogo

------------------------------------ INFORMAÇÕES SOBRE O JOGO -----------------------------------------

pegaPeca :: Tabuleiro -> Posicao -> Maybe Peca
pegaPeca tabuleiro (x, y) 
        | x >= 0 && x <= 7 && y >= 0 && y <= 7 = getPecaCasa ((tabuleiro !! y) !! x)
        | otherwise = Nothing

gerarCaminho :: Posicao -> Posicao -> [Posicao]
gerarCaminho (x1, y1) (x2, y2)
    | x1 == x2 = removeExtremos $ [(x1, y) | y <- [y1, y1 + stepY .. y2]]
    | y1 == y2 = removeExtremos $ [(x, y1) | x <- [x1, x1 + stepX .. x2]]
    | otherwise = reverse $ tail $ reverse $ zip [x1 + stepX, x1 + 2*stepX .. x2] [y1 + stepY, y1 + 2*stepY .. y2]
            where
                deltaX = abs (x2 - x1)
                deltaY = abs (y2 - y1)
                stepX = if x2 > x1 then 1 else -1
                stepY = if y2 > y1 then 1 else -1
                removeExtremos [] = []
                removeExtremos (x:xs) =  reverse $ tail $ reverse xs


encontrarPosicaoRei :: Tabuleiro -> Cor -> Posicao
encontrarPosicaoRei tabuleiro corRei = head $ filter encontraRei [(x,y) | y <- [0..7], x <- [0..7]]
        where
            encontraRei pos = case pegaPeca tabuleiro pos of 
                Just (Peca Rei cor) -> cor == corRei
                _ -> False

todasPosicoesAdversarios :: Tabuleiro -> Cor -> [Posicao]
todasPosicoesAdversarios tabuleiro corRei = filter pecasAdversarias [(x,y) | y <- [0..7], x <- [0..7]]
    where
        pecasAdversarias pos = case pegaPeca tabuleiro pos of
            Just (Peca _ corAdversario) -> corAdversario /= corRei
            Nothing -> False


todasPosicoesPropria :: Tabuleiro -> Cor -> [Posicao]
todasPosicoesPropria tabuleiro minhaCor = todasPosicoesAdversarios tabuleiro (trocarTurno minhaCor)

todasJogadasPossiveis :: Jogo -> Cor -> [(Posicao, Posicao)]
todasJogadasPossiveis jogo cor = movimentosPossiveis
    where
        produtoCartesiano xs ys = [(x, y) | x <- xs, y <- ys]
        todasPosicoesIniciais :: [Posicao]
        todasPosicoesIniciais = todasPosicoesPropria (getTabuleiro jogo) cor
        todosMovimentos = produtoCartesiano todasPosicoesIniciais (produtoCartesiano [0..7] [0..7])
        movimentosPossiveis = filter (\(posI, posF) -> getBoolRetorno(isMovimentoValido jogo posI posF)) todosMovimentos


trocarTurno :: Cor -> Cor
trocarTurno Branco = Preto
trocarTurno Preto = Branco

---------------------------------------- VALIDAÇÕES FIM DE JOGO --------------------------------------------------

isEmpate :: Jogo -> Bool
isEmpate jogo = length posicoesTime1 == length posicoesTime2 && length posicoesTime1 == 1
    where
        posicoesTime1 = todasPosicoesAdversarios (getTabuleiro jogo) Branco
        posicoesTime2 = todasPosicoesAdversarios (getTabuleiro jogo) Preto


isReiAfogado :: Jogo -> Bool
isReiAfogado jogo = semFulga && (length $ todasJogadasPossiveis jogo (getTurno jogo)) == 0
    where
        (xRei, yRei) = encontrarPosicaoRei (getTabuleiro jogo) (getTurno jogo)
        fugasRei = filter (\(x,y) -> x >= 0 && x <= 7 && y >= 0 && y <= 7) [(xRei -1, yRei) , (xRei-1, yRei+1), (xRei, yRei + 1), (xRei+1, yRei+1), (xRei+1, yRei), (xRei+1, yRei-1), (xRei, yRei-1), (xRei-1, yRei-1)]
        semFulga = not $ any (\mov -> getBoolRetorno (isMovimentoValido jogo (xRei, yRei) mov) ) fugasRei

isCheckMate :: Jogo -> Bool
isCheckMate jogo = isReiInCheck jogo (getTurno jogo) && isReiAfogado jogo


isReiInCheck :: Jogo -> Cor -> Bool
isReiInCheck jogo corRei = any (\pos -> getBoolRetorno(movimentoValidoPeca jogo pos posRei)) posicoesAdversarios
    where
        posicoesAdversarios = todasPosicoesAdversarios (getTabuleiro jogo) corRei
        posRei = encontrarPosicaoRei (getTabuleiro jogo) corRei



---------------------------------------- VALIDAÇÕES PARA JOGADA -------------------------------------------- 
caminhoLivre :: Tabuleiro -> Posicao -> Posicao -> Bool
caminhoLivre tabuleiro (xi, yi) (xf, yf)  = verificaVazia $ caminhos
    where
        caminhos = case pegaPeca tabuleiro (xi, yi) of 
            Just (Peca Peao _) -> if xi == xf 
                then (xf, yf) : gerarCaminho (xi, yi) (xf, yf) 
                else gerarCaminho (xi, yi) (xf, yf)
            _ ->  gerarCaminho (xi, yi) (xf, yf)
        verificaVazia xs = all isCasaVazia xs
        verificaVazia [] = True
        isCasaVazia (x, y) = case pegaPeca tabuleiro (x, y) of
            Just _ -> False
            Nothing -> True

isMovimentoBispoValido :: Posicao -> Posicao -> Bool
isMovimentoBispoValido (x0, y0) (xf, yf) = abs (xf - x0) == abs (yf - y0) 

isMovimentoCavaloValido :: Posicao -> Posicao -> Bool
isMovimentoCavaloValido (x0, y0) (xf, yf) = 
    abs(xf - x0) == 2 && abs(yf - y0) == 1 ||
    abs(xf - x0) == 1 && abs(yf - y0) == 2

isMovimentoReiValido ::  Posicao -> Posicao -> Bool
isMovimentoReiValido (x0, y0) (xf, yf) = maximum  [abs (xf - x0), abs (yf - y0)] == 1

isMovimentoTorreValido ::  Posicao -> Posicao -> Bool
isMovimentoTorreValido (x0, y0) (xf, yf) = 
    abs(xf - x0) >= 1 && abs(yf - y0) == 0 ||
    abs(xf - x0) == 0 && abs(yf - y0) >= 1

isMovimentoRainhaValido :: Posicao -> Posicao -> Bool
isMovimentoRainhaValido pI pF = 
    isMovimentoBispoValido pI pF || isMovimentoTorreValido pI pF

isMovimentoPeaoValido :: Tabuleiro -> Posicao -> Posicao -> Bool
isMovimentoPeaoValido tabuleiro (x0, y0) (xf, yf) =
    case pegaPeca tabuleiro (x0, y0) of
        Just peca ->
            let corPeca = getCor peca
            in case corPeca of
                Branco ->
                    if y0 == 6
                        then ((y0 - yf == 1 || y0 - yf == 2) && (x0 - xf) == 0) || movPeaoCaptura Branco
                        else (y0 - yf == 1 && (x0 - xf) == 0) || movPeaoCaptura Branco
                Preto ->
                    if y0 == 1
                        then ((yf - y0 == 1 || yf - y0 == 2) && (x0 - xf) == 0) || movPeaoCaptura Preto
                        else (yf - y0 == 1 && (x0 - xf) == 0) || movPeaoCaptura Preto
                where
                    movPeaoCaptura Branco = (y0 - yf == 1) && (abs(x0 - xf) == 1) && pecaOutraCor Branco
                    movPeaoCaptura Preto = (yf - y0 == 1) && (abs(x0 - xf) == 1) && pecaOutraCor Preto
                    pecaOutraCor cor = case pegaPeca tabuleiro (xf, yf) of
                        Just (Peca t corPeca) -> cor /= corPeca
                        Nothing -> False
                        
        Nothing -> False


movimentoValidoPeca :: Jogo -> Posicao -> Posicao -> Retorno
movimentoValidoPeca jogo posI posF = 
    case pegaPeca(getTabuleiro jogo) posI of
       Nothing -> Retorno False PecaNaoEncontrada
       Just (Peca Bispo _) ->    
           if isMovimentoBispoValido posI posF
               then if caminhoLivre (getTabuleiro jogo) posI posF
                        then Retorno True MovimentoValido
                        else Retorno False CaminhoBarrado
               else Retorno False MovimentoInvalidoPeca
       Just (Peca Cavalo _) ->   
           if isMovimentoCavaloValido posI posF
               then Retorno True MovimentoValido
               else Retorno False MovimentoInvalidoPeca
       Just (Peca Rei _) ->      
           if isMovimentoReiValido posI posF
               then if caminhoLivre (getTabuleiro jogo) posI posF
                        then Retorno True MovimentoValido
                        else Retorno False CaminhoBarrado
               else Retorno False MovimentoInvalidoPeca
       Just (Peca Torre _) ->    
           if isMovimentoTorreValido posI posF
               then if caminhoLivre (getTabuleiro jogo) posI posF
                        then Retorno True MovimentoValido
                        else Retorno False CaminhoBarrado
               else Retorno False MovimentoInvalidoPeca
       Just (Peca Rainha _) ->   
           if isMovimentoRainhaValido posI posF
               then if caminhoLivre (getTabuleiro jogo) posI posF
                        then Retorno True MovimentoValido
                        else Retorno False CaminhoBarrado
               else Retorno False MovimentoInvalidoPeca
       Just (Peca Peao _) ->     
           if isMovimentoPeaoValido (getTabuleiro jogo) posI posF
               then if caminhoLivre (getTabuleiro jogo) posI posF
                        then Retorno True MovimentoValido
                        else Retorno False CaminhoBarrado
               else Retorno False MovimentoInvalidoPeca


capturaPropriaPeca :: Tabuleiro -> Posicao -> Posicao -> Bool
capturaPropriaPeca tabuleiro (x0, y0) (xf, yf) =
    case (pegaPeca tabuleiro (x0, y0), pegaPeca tabuleiro (xf, yf)) of
        (Just peca, Just pecaDestino) -> 
            let corPeca = getCor peca
                corPecaDestino = getCor pecaDestino
            in corPeca == corPecaDestino
        _ -> False

movimentoForaTabuleiro :: Tabuleiro -> Posicao -> Posicao -> Bool
movimentoForaTabuleiro tabuleiro (x0, y0) (xf, yf) = x0 > 7 || x0 < 0 
    || xf > 7 || xf < 0 || y0 > 7 || y0 < 0 || yf > 7 || yf < 0


isTurnoCorreto :: Jogo -> Posicao -> Posicao -> Bool
isTurnoCorreto jogo posI posF = 
    case pegaPeca (getTabuleiro jogo) posI of
        Just (Peca tipo corPeca) -> corPeca == getTurno jogo
        _ -> False

isMovimentoSeguro :: Jogo -> Posicao -> Posicao -> Bool
isMovimentoSeguro jogo posInicial posFinal =
    let tabuleiroAposMovimento = realizarMovimento jogo posInicial posFinal
    in not (isReiInCheck (Jogo (getTurno jogo) tabuleiroAposMovimento (getResultadoUltimaJogada jogo) ) (getTurno jogo))

isAlgumaPecaNaPosI :: Jogo -> Posicao -> Bool
isAlgumaPecaNaPosI jogo posI  = 
    case pegaPeca(getTabuleiro jogo) posI of
        Just _ -> True
        Nothing -> False
        

isMovimentoValido :: Jogo -> Posicao -> Posicao -> Retorno
isMovimentoValido jogo posI posF = mconcat [vForaTabuleiro, vPeca, vTurno,
    vCapturaProprioTime, movimentoValidoPeca jogo posI posF, vSeguro]
    where
        vPeca = if(isAlgumaPecaNaPosI jogo posI)
            then Retorno True MovimentoValido
            else Retorno False PecaNaoEncontrada
        vTurno = if (isTurnoCorreto jogo posI posF)
            then Retorno True MovimentoValido
            else Retorno False MovimentoForaTurno
        vForaTabuleiro = if(not (movimentoForaTabuleiro (getTabuleiro jogo) posI posF))
            then Retorno True MovimentoValido
            else Retorno False MovimentoForaTabuleiro
        vCapturaProprioTime = if (not (capturaPropriaPeca (getTabuleiro jogo) posI posF))
            then Retorno True MovimentoValido
            else Retorno False ProprioTime
        vSeguro = if (isMovimentoSeguro jogo posI posF)
            then Retorno True MovimentoValido
            else Retorno False ProprioReiCheck
            
---------------------- REALIZA O MOVIMENTO NO TABULEIRO -------------------------------------------------

realizarMovimento :: Jogo -> Posicao -> Posicao -> Tabuleiro
realizarMovimento jogo@(Jogo turno tabuleiro _) (x0, y0) (xf, yf) =
    case pegaPeca tabuleiro (x0, y0) of
        Just peca -> atualizarTabuleiro tabuleiro (x0, y0) (Empty (x0, 7-y0)) (xf, yf) (Ocupada peca (xf, 7-yf))
        Nothing -> getTabuleiro jogo

atualizarTabuleiro :: Tabuleiro -> Posicao -> Casa -> Posicao -> Casa -> Tabuleiro
atualizarTabuleiro tabuleiro posI casaI posF casaF =
    [[if (x, y) == posI then casaI else if (x, y) == posF then casaF else tabuleiro !! y !! x | x <- [0..7]] | y <- [0..7]]


