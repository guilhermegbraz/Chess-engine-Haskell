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
