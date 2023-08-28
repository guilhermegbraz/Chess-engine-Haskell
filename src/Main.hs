module Main (main) where

import XadrezElementos
import XadrezPartida 
import IAXadrez
import System.IO

parserPosicao :: Posicao -> Posicao
parserPosicao (x, y) = (x, 7-y)

readDigito :: IO Int
readDigito = do
    input <- getLine
    let x = head input
    return (read [x])

readPosition :: IO Posicao
readPosition = do
    input <- getLine
    let [x, y] = words input
    return (read x, read y)

main :: IO ()
main = do

  hSetBuffering stdout NoBuffering
  putStrLn "Bem-vindo ao jogo de xadrez!"
  putStrLn "Primeiramente vamos escolher qual modo de jogo você quer jogar:"
  putStrLn "|||| Digite '1' para jogar contra um segundo jogador na mesma máquina, vocês irão digitar seus movimentos alternadamente"
  putStrLn "|||| Digite '2' para jogar contra uma IA que irá fazer movimentos aleatórios"
  entrada <- readDigito
  if entrada == 1 then loop iniciaJogo else loopIA iniciaJogo


loopIA :: Jogo -> IO()
loopIA jogo = do
      putStrLn $ show jogo
      case jogoTerminou jogo of
        True ->  putStrLn "Obrigado por jogar"
        False -> do 
            putStrLn "Informe a posição da peça que deseja mover (x y):"
            posI <- readPosition
            putStrLn "Informe a posição para onde deseja mover a peça (x y):"
            posF <- readPosition
            let jogoIA = jogada jogo (parserPosicao posI) (parserPosicao posF)
            if (getTurno jogoIA) == Preto
              then do
                  putStrLn $ show jogoIA
                  let (posInicioIA, posFinalIA) = getJogadaAleatoria jogoIA (getTurno jogoIA)
                  putStrLn "A jogada da IA foi"
                  putStrLn $ show  (parserPosicao posInicioIA, parserPosicao posFinalIA)
                  loopIA $ jogada jogoIA posInicioIA posFinalIA
              else loopIA jogoIA
           

loop :: Jogo -> IO ()
loop jogo = do
    putStrLn $ show jogo
    case jogoTerminou jogo of
      True -> putStrLn "Obrigado por jogar"
      False -> do 
          putStrLn "Informe a posição da peça que deseja mover (x y):"
          posI <- readPosition
          putStrLn "Informe a posição para onde deseja mover a peça (x y):"
          posF <- readPosition
          loop $ jogada jogo (parserPosicao posI) (parserPosicao posF)
        


