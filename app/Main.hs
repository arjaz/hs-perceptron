module Main where

import Perceptron

main :: IO ()
main = do
  putStrLn "Perceptron"
  let rate = 0.1
  let threshold = 1
  let iterations = 100
  let tests =
        [ ([0.0, 0.0], False)
        , ([1.0, 1.5], False)
        , ([2.0, 3.0], True)
        , ([5.0, 4.0], True)
        ]
  let (results, weights) = solve tests rate threshold iterations
  putStrLn "Expected classes:"
  putStrLn . unwords . map (show . snd) $ tests
  putStrLn "Actual classes:"
  putStrLn . unwords . map show $ results
  putStrLn "Weights:"
  putStrLn . unwords . map show $ weights
