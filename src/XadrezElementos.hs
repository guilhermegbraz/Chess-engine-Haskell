module XadrezElementos where

data TipoPeca = Peao | Torre | Cavalo | Bispo | Rei | Rainha
    deriving (Show, Eq)

data Cor = Branco | Preto
    deriving (Show, Eq)

type Posicao = (Int, Int)

data Peca =  Peca TipoPeca Cor

data Casa = Empty Posicao | Ocupada Peca Posicao

type Tabuleiro = [[Casa]]


data Jogo = Jogo Cor Tabuleiro

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

instance Show Casa where
    show (Empty (x, y)) 
        | (x + y) `mod` 2 == 0 = " ■ " 
        | otherwise = " □ " 
    
    show (Ocupada peca pos) = show peca

showTabuleiro :: Tabuleiro -> String
showTabuleiro = unlines . map (concatMap show) 

instance Show Jogo where
    show (Jogo Branco tabuleiro) = "Brancas jogam \n\n" ++ showTabuleiro tabuleiro
    show (Jogo Preto tabuleiro) = "Pretas jogam \n\n" ++ showTabuleiro tabuleiro