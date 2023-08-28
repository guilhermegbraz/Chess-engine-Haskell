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

    -- let jogo = iniciaJogo
    -- print $ jogo
    -- print $ "Rei afogado:"
    -- print $ isReiAfogado jogo
    -- let jogo2 = jogada jogo (parserPosicao (4,1)) (parserPosicao (4,3))
    -- print $ jogo2
    -- print $ "Rei afogado:"
    -- print $ isReiAfogado jogo2
    -- let jogo3 = jogada jogo2 (parserPosicao (3,6)) (parserPosicao (3,5))
    -- print $ jogo3
    -- print $ "Rei afogado:"
    -- print $ isReiAfogado jogo3
    -- let jogo4 = jogada jogo3 (parserPosicao (5,0)) (parserPosicao (1,4))
    -- print $ jogo4
    -- print $ encontrarPosicaoRei2 (getTabuleiro jogo4) Branco
    -- print $ encontrarPosicaoRei2 (getTabuleiro jogo4) Preto
    -- print $ isReiInCheck (jogo4) (getTurno jogo4)
    -- print $ "Rei afogado:"
    -- print $ isReiAfogado jogo4
    -- let jogo5 = jogada jogo4 (parserPosicao (4,6)) (parserPosicao (4,5))
    -- print $ jogo5
    -- print $ encontrarPosicaoRei2 (getTabuleiro jogo5) Branco
    -- print $ encontrarPosicaoRei2 (getTabuleiro jogo5) Preto
    -- print $ "Rei afogado:"
    -- print $ isReiAfogado jogo5
    
    -- let jogo = iniciaJogo
    -- print jogo
    -- print $ gerarCaminho (parserPosicao (3,0)) (parserPosicao (0,3))
    -- print $ parserPosicao (3,0)
    -- print $ parserPosicao (0,3)
