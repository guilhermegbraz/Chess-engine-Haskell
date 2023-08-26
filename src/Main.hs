module Main (main) where

import XadrezElementos
import XadrezPartida

parserPosicao :: Posicao -> Posicao
parserPosicao (x, y) = (x, 7-y)



main :: IO ()
main = do
  putStrLn "hello world, Jogo xadrez\n"
  let jogo = iniciaJogo 
  print(jogo)
  -- print $ pegaPeca (getTabuleiro jogo) $ parserPosicao (0,0)
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,1))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,2))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,3))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,1)) (parserPosicao(0,2))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,0))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,4))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,5))
  print $ (parserPosicao(3,3)) 
  print $ (parserPosicao(3,4))


