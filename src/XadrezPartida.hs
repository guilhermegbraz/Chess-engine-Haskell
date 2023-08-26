module XadrezPartida where

import XadrezElementos

iniciaJogo :: Jogo
iniciaJogo = Jogo Branco tabuleiroInicial

tabuleiroInicial :: Tabuleiro
tabuleiroInicial = [
        [Ocupada ((map (criaPecas Preto) [Torre, Cavalo, Bispo, Rainha, Rei, Bispo, Cavalo, Torre]) !! x ) (x,7)| x <- [0..7]]
        ,[Ocupada (Peca Peao Preto) (x,6) | x <- [0..7]]
    ] 
    ++
    reverse [[Empty (x,y) | y <- [0..7]] |  x <- [2..5]]
    ++
    [
        [Ocupada (Peca Peao Branco) (x,1) | x <- [0..7]]
        ,[Ocupada ((map (criaPecas Branco) [Torre, Cavalo, Bispo, Rainha, Rei, Bispo, Cavalo, Torre]) !! x ) (x,7)| x <- [0..7]]
    ]
    
    where
        criaPecas :: Cor -> TipoPeca -> Peca
        criaPecas cor tipo = Peca tipo cor

pegaPeca :: Tabuleiro -> Posicao -> Maybe Peca
pegaPeca tabuleiro (x, y) 
        | x >= 0 && x <= 7 && y >= 0 && y <= 7 = getPecaCasa ((tabuleiro !! y) !! x)
        | otherwise = Nothing

caminhoLivre :: Tabuleiro -> Posicao -> Posicao -> Bool
caminhoLivre tabuleiro posI posF  = all isCasaVazia $ gerarCaminho posI posF
    where
        isCasaVazia (x, y) = case pegaPeca tabuleiro (x, y) of
            Just _ -> False
            Nothing -> True

gerarCaminho :: Posicao -> Posicao -> [Posicao]
gerarCaminho (x1, y1) (x2, y2) = filter desconsideraOrigemFim caminho 
    where
      caminho = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
      desconsideraOrigemFim = \(x, y) -> (x,y) /= (x1, y1) && (x, y) /= (x2, y2)


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
                        then (yf - y0 == 1 || yf - y0 == 2 && (x0 - xf) == 0) || movPeaoCaptura Preto
                        else (yf - y0 == 1 && (x0 - xf) == 0) || movPeaoCaptura Preto
                where
                    movPeaoCaptura Branco = (y0 - yf == 1) && (abs(x0 - xf) == 1) && pecaOutraCor Branco
                    movPeaoCaptura Preto = (yf - y0 == 1) && (abs(x0 - xf) == 1) && pecaOutraCor Preto
                    pecaOutraCor cor = case pegaPeca tabuleiro (xf, yf) of
                        Just (Peca t corPeca) -> cor /= corPeca
                        Nothing -> False
                        
        Nothing -> False


movimentoValidoPeca :: Jogo -> Posicao -> Posicao -> Bool
movimentoValidoPeca jogo posI posF = 
    case pegaPeca(getTabuleiro jogo) posI of
       Nothing -> False
       Just (Peca Bispo _) ->    isMovimentoBispoValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
       Just (Peca Cavalo _) ->   isMovimentoCavaloValido posI posF
       Just (Peca Rei _) ->      isMovimentoReiValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
       Just (Peca Torre _) ->    isMovimentoTorreValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
       Just (Peca Rainha _) ->   isMovimentoRainhaValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
       Just (Peca Peao _) ->     isMovimentoPeaoValido (getTabuleiro jogo) posI posF  && caminhoLivre (getTabuleiro jogo) posI posF

capturaPropriaPeca :: Tabuleiro -> Posicao -> Posicao -> Bool
capturaPropriaPeca tabuleiro (x0, y0) (xf, yf) =
    case (pegaPeca tabuleiro (x0, y0), pegaPeca tabuleiro (xf, yf)) of
        (Just peca, Just pecaDestino) -> 
            let corPeca = getCor peca
                corPecaDestino = getCor pecaDestino
            in corPeca == corPecaDestino
                --if corPeca == corPecaCasa 
                --    then True
                --else 
                --    False
        --Nothing -> False
        _ -> False


movimentoForaTabuleiro :: Tabuleiro -> Posicao -> Posicao -> Bool
movimentoForaTabuleiro tabuleiro (x0, y0) (xf, yf) =
    case pegaPeca tabuleiro (x0, y0) of
        Just peca ->
                    if xf > 7 || xf < 0 || yf > 7 || yf < 0 
                        then True
                    else
                        False
        Nothing -> False

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

isTurnoCorreto :: Jogo -> Posicao -> Posicao -> Bool
isTurnoCorreto jogo posI posF = 
    case pegaPeca (getTabuleiro jogo) posI of
        Just (Peca tipo corPeca) -> corPeca == getTurno jogo
        _ -> False

-- para validar movimento, o movimento não pode:
-- 	dar check descoberto
-- 	comer peça da mesma cor
-- 	ir pra fora do tabuleiro
-- 	atropelar uma peça (exceto pelo cavalo)



isMovimentoValido :: Jogo -> Posicao -> Posicao -> Bool
isMovimentoValido jogo posI posF = 
    isTurnoCorreto jogo posI posF && 
    not movimentoForaTabuleiro &&
    not capturaPropriaPeca && 
    movimentoValidoPeca jogo posI posF && 
    (isMovimentoSeguro jogo posI posF)
    where
        isMovimentoSeguro _ _ _ = True -- Validar se o movimento deixa o rei sobre ataque
        capturaPropriaPeca = False -- Implemente a lógica para verificar captura de peça da mesma cor
        movimentoForaTabuleiro = False -- Implemente a lógica para verificar movimento fora do tabuleiro
        

isReiInCheck :: Jogo -> Cor -> Bool
isReiInCheck jogo corRei = any (\pos -> movimentoValidoPeca jogo pos posRei) posicoesAdversarios
    where
        posicoesAdversarios = todasPosicoesAdversarios (getTabuleiro jogo) corRei
        posRei = encontrarPosicaoRei (getTabuleiro jogo) corRei


-- isMovimentoSeguro :: Tabuleiro -> Cor -> Posicao -> Posicao -> Bool
-- isMovimentoSeguro tabuleiro corRei posInicial posFinal =
--     let tabuleiroAposMovimento = realizarMovimento tabuleiro posInicial posFinal
--     in not (isReiInCheck tabuleiroAposMovimento corRei)

-- checkDescoberto :: Tabuleiro -> Cor -> Posicao -> Posicao -> Bool
-- checkDescoberto tabuleiro corRei posAtacante posRei =
--     case pegaPeca tabuleiro posAtacante of
--         Just peca ->
--             let possiveisAtacantes = todasPosicoesAdversarios tabuleiro corRei
--             in any (\posPossivelAtacante -> isMovimentoValido tabuleiro corRei posPossivelAtacante posRei && caminhoLivre tabuleiro posPossivelAtacante posAtacante) possiveisAtacantes
--         Nothing -> False