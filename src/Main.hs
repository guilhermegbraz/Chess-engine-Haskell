module Main (main) where

import XadrezElementos
import XadrezPartida

main :: IO ()
main = do
  putStrLn "hello world, Jogo xadrez\n"
  print(iniciaJogo)

