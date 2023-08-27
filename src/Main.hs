module Main (main) where

import XadrezElementos
import XadrezPartida 
import System.IO

parserPosicao :: Posicao -> Posicao
parserPosicao (x, y) = (x, 7-y)


isEntradaInvalida:: Posicao -> Posicao -> Bool 
isEntradaInvalida (x0, y0) (xf, yf) = maximum [x0, y0, xf, yf] > 7 || minimum [x0, y0, xf, yf] < 0

main :: IO ()
main = do
  let jogo = iniciaJogo 
  let possiveisJogadas = todasJogadasPossiveis jogo Branco
  -- print jogo
  -- print $ gerarCaminho (parserPosicao (2,0)) (parserPosicao (0,2))
  -- print $ caminhoLivre (getTabuleiro jogo) (parserPosicao (2,0)) (parserPosicao (0,2))
  -- print $ possiveisJogadas
  -- print $ length possiveisJogadas
  hSetBuffering stdout NoBuffering
  putStrLn "Bem-vindo ao jogo de xadrez!"
  loop (jogoQuaseAfoado)


loop :: Jogo -> IO ()
loop jogo = do
    putStrLn $ show jogo
    case jogoTerminou jogo of
      True -> do putStrLn "Obrigado por jogar"
      False -> do 
          putStrLn "Informe a posição da peça que deseja mover (x y):"
          posI <- readPosition
          putStrLn "Informe a posição para onde deseja mover a peça (x y):"
          posF <- readPosition
          loop $ jogada jogo (parserPosicao posI) (parserPosicao posF)
        

readPosition :: IO Posicao
readPosition = do
    input <- getLine
    let [x, y] = words input
    return (read x, read y)

