module Main (main) where

import XadrezElementos
import XadrezPartida
import Movimentacao

parserPosicao :: Posicao -> Posicao
parserPosicao (x, y) = (x, 7-y)

-- loopJogo:: Jogo -> IO ()
-- loopJogo jogo = do 


main :: IO ()
main = do
  putStrLn "hello world, Jogo xadrez\n"
  let jogo = iniciaJogo 
  print(jogo)
  let novoJogo =jogada jogo (parserPosicao(0,1)) (parserPosicao(0,3))
  print(novoJogo)
  let novoJogo2 =jogada novoJogo (parserPosicao(1,6)) (parserPosicao(1,4))
  print(novoJogo2)
  let novoJogo3 =jogada novoJogo2 (parserPosicao(0,3)) (parserPosicao(1,4))
  print(novoJogo3)
  let novoJogo4 =jogada novoJogo3 (parserPosicao(1,7)) (parserPosicao(2,5))
  print(novoJogo4)
  -- -- print $ pegaPeca (getTabuleiro  jogo) $ parserPosicao (0,0)
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,1))
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,2))
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,3))
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,1)) (parserPosicao(0,2))
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,0))
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,4))
  -- print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,5))
  -- print $ (parserPosicao(3,3)) 
  -- print $ (parserPosicao(3,4))  
  -- putStrLn "Teste fora do tabuleiro"
  -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,5))
  -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,9))
  -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(-2,5))
  -- print $ movimentoForaTabuleiro (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(8,2))
  -- putStrLn "Teste Captura Propria Peca"
  -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(1,1))
  -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(6,1))
  -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(6,3))
  -- print $ capturaPropriaPeca (getTabuleiro jogo) (parserPosicao(1,1)) (parserPosicao(3,5))
  -- -- print $ pegaPeca (getTabuleiro jogo) $ parserPosicao  (0,0)
  -- print $ isReiInCheck jogo Branco 
 

