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

-- tabuleiroInicial :: Tabuleiro
-- tabuleiroInicial = [
--     [ criaPecaPreto Torre 0 7, criaPecaPreto Cavalo 1 7, criaPecaPreto Bispo 2 7, criaPecaPreto Rainha 3 7
--     , criaPecaPreto Rei 4 7, criaPecaPreto Bispo 5 7, criaPecaPreto Cavalo 6 7, criaPecaPreto Torre 7 7
--     ],
--     [ criaPecaPreto Peao x 6 | x <- [0..7] ]
--     ] ++
--     [ replicate 8 (Empty (x, y)) | y <- [2..5], x <- [0..7] ] ++
--     [
--     [ criaPecaBranco Peao x 1 | x <- [0..7] ],
--     [ criaPecaBranco Torre 0 0, criaPecaBranco Cavalo 1 0, criaPecaBranco Bispo 2 0, criaPecaBranco Rainha 3 0
--     , criaPecaBranco Rei 4 0, criaPecaBranco Bispo 5 0, criaPecaBranco Cavalo 6 0, criaPecaBranco Torre 7 0
--     ]
--     ]
--     where
--         criaPecaPreto :: TipoPeca -> Int -> Int -> Casa
--         criaPecaPreto tipo x y = Ocupada (Peca tipo Preto) (x, y)
        
--         criaPecaBranco :: TipoPeca -> Int -> Int -> Casa
--         criaPecaBranco tipo x y = Ocupada (Peca tipo Branco) (x, y)
