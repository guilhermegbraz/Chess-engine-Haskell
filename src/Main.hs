module Main (main) where

import XadrezElementos
import XadrezPartida 
import System.IO

parserPosicao :: Posicao -> Posicao
parserPosicao (x, y) = (x, 7-y)

-- -- loopJogo:: Jogo -> IO ()
-- -- loopJogo jogo = do 


-- main :: IO ()
-- main = do
--   putStrLn "hello world, Jogo xadrez\n"
--   let jogo = iniciaJogo 
--   print(jogo)
--   let novoJogo =jogada jogo (parserPosicao(0,1)) (parserPosicao(0,3))
--   print(novoJogo)
--   let novoJogo2 =jogada novoJogo (parserPosicao(1,6)) (parserPosicao(1,4))
--   print(novoJogo2)
--   let novoJogo3 =jogada novoJogo2 (parserPosicao(0,3)) (parserPosicao(1,4))
--   print(novoJogo3)
--   let novoJogo4 =jogada novoJogo3 (parserPosicao(1,7)) (parserPosicao(2,5))
--   print(novoJogo4)
--   let novoJogo5 =jogada novoJogo4 (parserPosicao(2,0)) (parserPosicao(4,2))
--   print(novoJogo5)
--   -- -- print $ pegaPeca (getTabuleiro  jogo) $ parserPosicao (0,0)
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,1))
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,2))
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,3))
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,1)) (parserPosicao(0,2))
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,0))
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,4))
--   -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,5))
--   -- print $ (parserPosicao(3,3)) 
--   -- print $ (parserPosicao(3,4))  
--   -- putStrLn "Teste fora do tabuleiro"
--   -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,5))
--   -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,9))
--   -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(-2,5))
--   -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(8,2))
--   -- putStrLn "Teste Captura Propria Peca"
--   -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(1,1))
--   -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(6,1))
--   -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(6,3))
--   -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,5))
--   -- -- print $ pegaPeca (getTabuleiro jogo) $ parserPosicao  (0,0)
--   -- print $ isReiInCheck jogo Branco 

isEntradaInvalida:: Posicao -> Posicao -> Bool
isEntradaInvalida (x0, y0) (xf, yf) = maximum [x0, y0, xf, yf] > 7 || minimum [x0, y0, xf, yf] < 0

main :: IO ()
main = do
--   putStrLn "hello world, Jogo xadrez\n"
--   let jogo = iniciaJogo 
--   print(jogo)
--   -- print $ pegaPeca (getTabuleiro jogo) $ parserPosicao (0,0)

--   let afogado = isReiAfogado (jogo)
--   print(afogado)

--   let check = isCheckMate (jogo)
--   print(check)
  {-
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,1))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,2))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,3))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,1)) (parserPosicao(0,2))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,0))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,4))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,5))
  print $ (parserPosicao(3,3)) 
  print $ (parserPosicao(3,4))
  putStrLn "Teste fora do tabuleiro"
  print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,5))
  print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,9))
  print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(-2,5))
  print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(8,2))
  putStrLn "Teste Captura Propria Peca"
  print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(1,1))
  print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(6,1))
  print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(6,3))
  print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,5))
  -}
    hSetBuffering stdout NoBuffering
    putStrLn "Bem-vindo ao jogo de xadrez!"
    loop (iniciaJogo)
    -- let jogo = iniciaJogo
    -- print jogo
    -- print $ gerarCaminho (parserPosicao (3,0)) (parserPosicao (0,3))
    -- print $ parserPosicao (3,0)
    -- print $ parserPosicao (0,3)

loop :: Jogo -> IO ()
loop jogo = do
    putStrLn $ show jogo
    putStrLn "Informe a posição da peça que deseja mover (x y):"
    posI <- readPosition
    putStrLn "Informe a posição para onde deseja mover a peça (x y):"
    posF <- readPosition
    case isEntradaInvalida posI posF of
      True -> do
        putStrLn "Movimento inválido: entrada fora dos limites do tabuleiro."
        loop jogo
      False -> loop $ jogada jogo (parserPosicao posI) (parserPosicao posF)
                 
    

readPosition :: IO Posicao
readPosition = do
    input <- getLine
    let [x, y] = words input
    return (read x, read y)

