module XadrezElementos where

-- Elementos Base
data TipoPeca = Peao | Torre | Cavalo | Bispo | Rei | Rainha
    deriving (Show, Eq)

data Cor = Branco | Preto
    deriving (Show, Eq)

type Posicao = (Int, Int)

data TipoMovimento = MovimentoValido | ProprioReiCheck | ProprioTime | CaminhoBarrado | MovimentoInvalidoPeca

instance Show TipoMovimento where
    show ProprioReiCheck = "Esse movimento coloca o seu Rei em Check"
    show ProprioTime = "Esse movimento captura uma peça do seu proprio time"
    show CaminhoBarrado = "Há uma peça bloqueando o caminho para esse movimento"
    show MovimentoInvalidoPeca = "Movimento invalido. Essa peça não se move dessa forma"

-- ELEMENTOS JOGO ----------------------------

-------- Peca

data Peca =  Peca TipoPeca Cor

instance Show Peca where
    show (Peca Peao Branco) = " ♙ "
    show (Peca Peao Preto) = " ♟ "

    show (Peca Torre Branco) = " ♖ "
    show (Peca Torre Preto) = " ♜ "

    show (Peca Cavalo Branco) = " ♘ "
    show (Peca Cavalo Preto) = " ♞ "

    show (Peca Bispo Branco) = " ♗ "
    show (Peca Bispo Preto) = " ♝ "

    show (Peca Rei Branco) = " ♔ "
    show (Peca Rei Preto) = " ♚ "

    show (Peca Rainha Branco) = " ♕ "
    show (Peca Rainha Preto) = " ♛ "

getCor :: Peca -> Cor
getCor (Peca _ cor) = cor

-------- Casa
data Casa = Empty Posicao | Ocupada Peca Posicao

instance Show Casa where
    show (Empty (x, y)) 
        | (x + y) `mod` 2 == 0 = " ■ " 
        | otherwise = " □ " 
    
    show (Ocupada peca pos) = show peca

getPecaCasa :: Casa -> Maybe Peca
getPecaCasa (Empty _) = Nothing
getPecaCasa (Ocupada peca _) = Just peca
            

-------- Tabuleiro
type Tabuleiro = [[Casa]]

showTabuleiro :: Tabuleiro -> String
showTabuleiro tabuleiro =  "  0  1  2  3  4  5  6  7\n" ++ (unlines . map (concatMap show)) tabuleiro
    --(show "0  1  2  3  4  5  6  7\n") ++ (unlines . map (concatMap show))
-- showTabuleiro tabuleiro = unlines $ colNumbers : separator : (zipWith (\i row -> show i ++ " |" ++ concatMap show row) [1..] tabLines)
--   where
--     colNumbers = "    " ++ concatMap (\i -> " " ++ show i ++ " ") [1..8]
--     separator = "   " ++ replicate 33 '-'
--     tabLines = map (\(i, row) -> show i ++ " |" ++ row) $ zip [8,7..1] (map (concatMap show) tabuleiro)

-------- Jogo
data Jogo = Jogo Cor Tabuleiro

getTurno :: Jogo -> Cor
getTurno (Jogo turno tabuleiro) = turno

getTabuleiro :: Jogo -> Tabuleiro
getTabuleiro (Jogo turno tabuleiro) = tabuleiro

instance Show Jogo where
    show (Jogo Branco tabuleiro) = "Brancas jogam \n\n" ++ showTabuleiro tabuleiro
    show (Jogo Preto tabuleiro) = "Pretas jogam \n\n" ++ showTabuleiro tabuleiro

