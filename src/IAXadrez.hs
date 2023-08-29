module IAXadrez (
    getJogadaAleatoria
) where

import System.Random
import XadrezPartida
import XadrezElementos

gerarNumeroAleatorio :: Int -> Int -> Int
gerarNumeroAleatorio min max = let gen = mkStdGen 42  -- Use uma seed adequada
                               in fst $ randomR (min, max-1) gen

getJogadaAleatoria :: Jogo -> Cor -> (Posicao, Posicao)
getJogadaAleatoria jogo cor =  jogadasPossiveis !! indiceAleatorio
    where 
        jogadasPossiveis = todasJogadasPossiveis jogo cor
        indiceAleatorio = gerarNumeroAleatorio 0 $ length jogadasPossiveis
