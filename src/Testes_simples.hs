module Testes_Simples (main) where

import XadrezElementos
import XadrezPartida

parserPosicao :: Posicao -> Posicao
parserPosicao (x, y) = (x, 7-y)



main :: IO ()
main = do
  putStrLn "hello world, Jogo xadrez\n"
  let jogo = iniciaJogo 
  print(jogo)
  print $ pegaPeca (getTabuleiro jogo) $ parserPosicao (0,0)
  print $ isMovimentoBispoValido (0, 0) (7, 7)
  print $ isMovimentoBispoValido (7, 5) (2, 0)
  print $ isMovimentoBispoValido (2, 0) (3, 5)
  print $ pegaPeca (getTabuleiro jogo) $ parserPosicao (7,7)
  print $ caminhoLivre (getTabuleiro jogo) (parserPosicao(0, 2)) $ parserPosicao (2, 7)
  print $ gerarCaminho (parserPosicao(0, 2)) $ parserPosicao (2, 7)

  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,1))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,2))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,3))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,1)) (parserPosicao(0,2))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(0,1)) (parserPosicao(0,0))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,4))
  print $ isMovimentoPeaoValido (getTabuleiro jogo) (parserPosicao(3,3)) (parserPosicao(3,5))
  print $ (parserPosicao(3,3)) 
  print $ (parserPosicao(3,4))
    print $ (isTurnoCorreto jogo (parserPosicao (0,1)) (parserPosicao (0,2))) == True
  print $ (isTurnoCorreto jogo (parserPosicao (0,6)) (parserPosicao (0,5))) == False
    print $ todasPosicoesAdversarios (getTabuleiro jogo) Branco
  print $ isTurnoCorreto jogo (parserPosicao (0,1)) (parserPosicao (0,2))
  print $ isTurnoCorreto jogo (parserPosicao (0,6)) (parserPosicao (0,5))


