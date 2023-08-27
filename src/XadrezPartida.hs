module XadrezPartida where

import XadrezElementos
import Movimentacao

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

pegaPeca :: Tabuleiro -> Posicao -> Maybe Peca
pegaPeca tabuleiro (x, y) 
        | x >= 0 && x <= 7 && y >= 0 && y <= 7 = getPecaCasa ((tabuleiro !! y) !! x)
        | otherwise = Nothing

encontrarPosicaoRei :: Tabuleiro -> Cor -> Posicao
encontrarPosicaoRei tabuleiro corRei = head $ filter encontraRei [(x,y) | y <- [0..7], x <- [0..7]]
        where
            encontraRei pos = case pegaPeca tabuleiro pos of 
                Just (Peca Rei cor) -> cor == corRei
                _ -> False


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


todasPosicoesAdversarios :: Tabuleiro -> Cor -> [Posicao]
todasPosicoesAdversarios tabuleiro corRei = filter pecasAdversarias [(x,y) | y <- [0..7], x <- [0..7]]
    where
        pecasAdversarias pos = case pegaPeca tabuleiro pos of
            Just (Peca _ corAdversario) -> corAdversario /= corRei
            Nothing -> False


isReiInCheck :: Jogo -> Cor -> Bool
isReiInCheck jogo corRei = any (\pos -> getBoolRetorno(movimentoValidoPeca jogo pos posRei)) posicoesAdversarios
    where
        posicoesAdversarios = todasPosicoesAdversarios (getTabuleiro jogo) corRei
        posRei = encontrarPosicaoRei (getTabuleiro jogo) corRei

jogada:: Jogo -> Posicao -> Posicao -> Jogo
jogada jogo posI posF = if retornoBool 
    then Jogo (trocarTurno (getTurno jogo)) (realizarMovimento jogo posI posF) retornoResultado
    else Jogo (getTurno jogo) (getTabuleiro jogo) retornoResultado
    where
        (Retorno retornoBool retornoResultado) = isMovimentoValido jogo posI posF


realizarMovimento :: Jogo -> Posicao -> Posicao -> Tabuleiro
realizarMovimento jogo@(Jogo turno tabuleiro _) (x0, y0) (xf, yf) =
    case pegaPeca tabuleiro (x0, y0) of
        Just peca -> atualizarTabuleiro tabuleiro (x0, y0) (Empty (x0, 7-y0)) (xf, yf) (Ocupada peca (xf, 7-yf))
        Nothing -> getTabuleiro jogo

atualizarTabuleiro :: Tabuleiro -> Posicao -> Casa -> Posicao -> Casa -> Tabuleiro
atualizarTabuleiro tabuleiro posI casaI posF casaF =
    [[if (x, y) == posI then casaI else if (x, y) == posF then casaF else tabuleiro !! y !! x | x <- [0..7]] | y <- [0..7]]

trocarTurno :: Cor -> Cor
trocarTurno Branco = Preto
trocarTurno Preto = Branco

------------------ VALIDAÇÕES ---------------------------------------------
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


-- movimentoValidoPeca :: Jogo -> Posicao -> Posicao -> Bool
-- movimentoValidoPeca jogo posI posF = 
--     case pegaPeca(getTabuleiro jogo) posI of
--        Nothing -> False
--        Just (Peca Bispo _) ->    isMovimentoBispoValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
--        Just (Peca Cavalo _) ->   isMovimentoCavaloValido posI posF
--        Just (Peca Rei _) ->      isMovimentoReiValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
--        Just (Peca Torre _) ->    isMovimentoTorreValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
--        Just (Peca Rainha _) ->   isMovimentoRainhaValido posI posF && caminhoLivre (getTabuleiro jogo) posI posF
--        Just (Peca Peao _) ->     isMovimentoPeaoValido (getTabuleiro jogo) posI posF  && caminhoLivre (getTabuleiro jogo) posI posF

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


isTurnoCorreto :: Jogo -> Posicao -> Posicao -> Bool
isTurnoCorreto jogo posI posF = 
    case pegaPeca (getTabuleiro jogo) posI of
        Just (Peca tipo corPeca) -> corPeca == getTurno jogo
        _ -> False

isMovimentoSeguro :: Jogo -> Posicao -> Posicao -> Bool
isMovimentoSeguro jogo posInicial posFinal =
    let tabuleiroAposMovimento = realizarMovimento jogo posInicial posFinal
    in not (isReiInCheck (Jogo (getTurno jogo) tabuleiroAposMovimento (getResultadoUltimaJogada jogo) ) (getTurno jogo))


isMovimentoValido :: Jogo -> Posicao -> Posicao -> Retorno
isMovimentoValido jogo posI posF = 
    vTurno <>  
    vForaTabuleiro <>
    vCapturaProprioTime <>
    movimentoValidoPeca jogo posI posF <>
    vSeguro
    where
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
