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
<<<<<<< HEAD
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
=======
  -- print $ pegaPeca (getTabuleiro jogo) $ parserPosicao  (0,0)
  print $ isReiInCheck jogo Branco
 
>>>>>>> 3c54b0d (testes)

