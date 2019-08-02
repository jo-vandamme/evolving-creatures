module NeuralNetTest where

import NeuralNet

xorNet :: NeuralNet Float
xorNet = NeuralNet [input, output]
    where input = Layer [[54, 17], [14, 14]] [-8, -20] sigmoid
          output = Layer [[92, -98]] [-48] sigmoid

testNN :: IO ()
testNN = do
    putStrLn "Xor Neural Network"
    print $ networkPredict xorNet [0, 0]
    print $ networkPredict xorNet [0, 1]
    print $ networkPredict xorNet [1, 0]
    print $ networkPredict xorNet [1, 1]
