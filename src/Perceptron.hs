module Perceptron
  ( solve
  ) where

type Data a = [([a], Bool)]

delta :: Num a => a -> a -> a
delta = (-)

predict :: Ord a => a -> a -> Bool
predict = (>)

output :: Num a => [(a, a)] -> a
output = sum . map (uncurry (*))

-- apply training on tests
train :: (Num a, Ord a) => Data a -> [a] -> a -> a -> Int -> [a]
train _ weights _ _ 0 = weights
train tests weights rate threshold iterations =
  train tests newWeights rate threshold (iterations - 1)
  where
    newWeights = trainTest tests weights rate threshold

-- apply training to tests[0] and pass the resulting weights to applying training to test[1] and so on
trainTest :: (Num a, Ord a) => Data a -> [a] -> a -> a -> [a]
trainTest [] weights _ _ = weights
trainTest (test:tests) weights rate threshold =
  trainTest tests newWeights rate threshold
  where
    newWeights =
      if expected /= prediction
        then zipWith (\w x -> w + x * rate * err) weights xs
        else weights
    xs = fst test
    expected = snd test
    prediction = predict y threshold
    y = output $ zip xs weights
    err = delta threshold y

solve :: (Num a, Ord a) => Data a -> a -> a -> Int -> ([Bool], [a])
solve tests rate threshold iterations = (results, weights)
  where
    results = map (\x -> predict (output $ zip (fst x) weights) threshold) tests
    weights = train tests startingWeights rate threshold iterations
    startingWeights = [0 | _ <- [1 .. length tests]]
