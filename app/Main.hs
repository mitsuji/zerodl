module Main where

import Prelude hiding (and,or)

data Perceptron = Perceptron { percW1 :: Float
                             , percW2 :: Float
                             , percB  :: Float
                             }


run :: Perceptron -> Float -> Float -> Float
run p x1 x2 =
  let
    w1 = percW1 p
    w2 = percW2 p
    b  = percB p
  in fromIntegral $ fromEnum $ b + w1 * x1 + w2 * x2 > 0


and :: Float -> Float -> Float
and = run $ Perceptron 0.5 0.5 (-0.7)

nand :: Float -> Float -> Float
nand = run $ Perceptron (-0.5) (-0.5) 0.7

or :: Float -> Float -> Float
or = run $ Perceptron 0.5 0.5 0.0

xor :: Float -> Float -> Float
xor x1 x2 =
  let
    s1 = nand x1 x2
    s2 = or x1 x2
  in and s1 s2

main :: IO ()
main = do
  print $ and 0.0 0.0
  print $ and 1.0 0.0
  print $ and 0.0 1.0
  print $ and 1.0 1.0
  
  print $ nand 0.0 0.0
  print $ nand 1.0 0.0
  print $ nand 0.0 1.0
  print $ nand 1.0 1.0
  
  print $ or 0.0 0.0
  print $ or 1.0 0.0
  print $ or 0.0 1.0
  print $ or 1.0 1.0
  
  print $ xor 0.0 0.0
  print $ xor 1.0 0.0
  print $ xor 0.0 1.0
  print $ xor 1.0 1.0
  
