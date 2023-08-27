module XadrezElementos where

-- Elementos Base
data TipoPeca = Peao | Torre | Cavalo | Bispo | Rei | Rainha
    deriving (Show, Eq)

data Cor = Branco | Preto
    deriving (Show, Eq)

type Posicao = (Int, Int)

data ResultadoJogada = MovimentoValido | CapturaValida | ProprioReiCheck | ReiEmCheck | ProprioTime | CaminhoBarrado | MovimentoInvalidoPeca | MovimentoForaTurno | MovimentoForaTabuleiro | PecaNaoEncontrada | PrimeiroMovimento | CheckMate

instance Show ResultadoJogada where
    show MovimentoValido  = "Movimento realizado"
    show ProprioReiCheck = "Movimento Inválido. Esse movimento coloca o seu Rei em Check"
    show ReiEmCheck = "Movimento Inválido. Seu rei esta em check, movimento o rei ou tome a peça que esta atacando-o"
    show ProprioTime = "Movimento Inválido. Esse movimento captura uma peça do seu proprio time"
    show CaminhoBarrado = "Movimento Inválido. Há uma peça bloqueando o caminho para esse movimento"
    show MovimentoInvalidoPeca = "Movimento invalido. Essa peça não se move dessa forma"
    show MovimentoForaTurno = "Movimento invalido. Não é a sua vez de jogar"
    show MovimentoForaTabuleiro = "Movimento invalido. Esse movimento ultrapassa os limites (fisicos) do tabuleiro"
    show PecaNaoEncontrada = "Movimento invalido. Não há peça alguma na posição inicial passada"
    show PrimeiroMovimento = "O jogo irá começar"
    show CheckMate = "Check Mate! Fim de jogo."


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
showTabuleiro tabuleiro =
    let verticalNumbers = "76543210\n"
        rowsWithNumbers = zipWith (\n row -> show n ++ concatMap show row) [0..7] tabuleiro
    in "  0  1  2  3  4  5  6  7\n" ++ unlines rowsWithNumbers


-------- Jogo
data Jogo = Jogo Cor Tabuleiro ResultadoJogada
--ResultadoJogada

getTurno :: Jogo -> Cor
getTurno (Jogo turno _ _) = turno

getTabuleiro :: Jogo -> Tabuleiro
getTabuleiro (Jogo _ tabuleiro _ ) = tabuleiro

getResultadoUltimaJogada :: Jogo -> ResultadoJogada
getResultadoUltimaJogada (Jogo _ _ resultado) = resultado

instance Show Jogo where
    show (Jogo Branco tabuleiro resultadoUltimaJogada) = "Brancas jogam \n\n" ++ showTabuleiro tabuleiro ++ "\n"  ++ show resultadoUltimaJogada ++ "\n" 
    show (Jogo Preto tabuleiro resultadoUltimaJogada ) = "Pretas jogam \n\n" ++ showTabuleiro tabuleiro  ++ "\n"  ++ show resultadoUltimaJogada ++ "\n"

data Retorno = Retorno Bool ResultadoJogada
    deriving Show

getResultadoJogada :: Retorno -> ResultadoJogada
getResultadoJogada (Retorno b r) = r

getBoolRetorno :: Retorno -> Bool
getBoolRetorno (Retorno b r) = b 

instance Semigroup Retorno where 
    (Retorno bool MovimentoValido) <> (Retorno b MovimentoForaTurno) = Retorno False MovimentoForaTurno
    (Retorno bool MovimentoForaTurno) <>  _ = Retorno False MovimentoForaTurno
    (Retorno bool MovimentoForaTabuleiro) <> _ = Retorno False MovimentoForaTabuleiro
    (Retorno bool MovimentoValido) <> (Retorno b MovimentoForaTabuleiro) = Retorno False MovimentoForaTabuleiro
    (Retorno bool MovimentoValido) <> (Retorno b ProprioTime) = Retorno False ProprioTime
    (Retorno bool ProprioTime) <> _ = Retorno False ProprioTime
    (Retorno bool MovimentoValido) <> (Retorno b MovimentoInvalidoPeca) = Retorno False MovimentoInvalidoPeca
    (Retorno bool MovimentoInvalidoPeca) <> _ = Retorno False MovimentoInvalidoPeca
    (Retorno bool MovimentoValido) <> (Retorno b CaminhoBarrado) = Retorno False CaminhoBarrado
    (Retorno b CaminhoBarrado) <> _ = Retorno False CaminhoBarrado
    (Retorno bool MovimentoValido) <> (Retorno b ProprioReiCheck) = Retorno False ProprioReiCheck
    (Retorno bool ProprioReiCheck) <> _ = Retorno False ProprioReiCheck
    (Retorno b1 MovimentoValido) <> (Retorno b2 MovimentoValido) = Retorno True MovimentoValido
    

instance Monoid Retorno where
    mempty = Retorno True MovimentoValido
    mappend = (<>)
