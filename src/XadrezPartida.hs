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
                        then (y0 - yf == 1 || y0 - yf == 2) && (x0 - xf) == 0
                        else y0 - yf == 1 && (x0 - xf) == 0
                Preto ->
                    if y0 == 1
                        then yf - y0 == 1 || yf - y0 == 2 && (x0 - xf) == 0
                        else yf - y0 == 1 && (x0 - xf) == 0
        Nothing -> False

checkDescoberto :: Bool
checkDescoberto = False

capturaPropriaPeca :: Bool
capturaPropriaPeca = False

movimentoForaTabuleiro :: Bool
movimentoForaTabuleiro = False

-- isReiEmCheck :: Tabuleiro -> Cor -> Bool

-- isReiInCheck :: Tabuleiro -> Cor -> Bool
-- isReiInCheck tabuleiro corRei =
--     let posRei = encontrarPosicaoRei tabuleiro corRei
--     in any (\posAdversario -> isMovimentoValido tabuleiro posAdversario posRei) posicoesAdversarios
--     where
--         posicoesAdversarios = todasPosicoesAdversarios tabuleiro corRei

-- encontrarPosicaoRei :: Tabuleiro -> Cor -> Posicao
-- encontrarPosicaoRei tabuleiro corRei =
--     head [pos | y <- [0..7], x <- [0..7], Just (Peca Rei cor) <- [pegaPeca tabuleiro (x, y)], cor == corRei]

-- todasPosicoesAdversarios :: Tabuleiro -> Cor -> [Posicao]
-- todasPosicoesAdversarios tabuleiro corRei =
--     [pos | y <- [0..7], x <- [0..7], Just (Peca _ cor) <- [pegaPeca tabuleiro (x, y)], cor /= corRei]

-- para validar movimento, o movimento não pode:
-- 	dar check descoberto
-- 	comer peça da mesma cor
-- 	ir pra fora do tabuleiro
-- 	atropelar uma peça (exceto pelo cavalo)