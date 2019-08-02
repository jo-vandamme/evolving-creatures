module NeuralNet where

import Control.Monad.Random
import Data.List

data Layer a = Layer { weights    :: [[a]]
                     , biases     :: [a]
                     , activation :: a -> a
                     }

data NeuralNet a = NeuralNet { layers :: [Layer a]
                             } deriving (Eq)

instance Eq a => Eq (Layer a) where
    a == b = weights a == weights b && biases a == biases b

instance Show a => Show (Layer a) where
    show l = "#  Layer " ++ show n1 ++ " x " ++ show n2 ++ "\nw: " ++
        show (weights l) ++ "\nb: " ++ show (biases l)
        where n1 = length $ (weights l) !! 0
              n2 = length $ weights l

instance Show a => Show (NeuralNet a) where
    show nn = intercalate "\n" $ map show $ layers nn

-- Activation functions

relu :: (Floating a, Ord a) => a -> a
relu a = if (a > 0) then a else 0

sigmoid :: Floating a => a -> a
sigmoid t = 1 / (1 + exp (-1 * t))

reclu :: Floating a => a -> a
reclu t = log (1 + exp t)

-- NN generation

randomLayer :: (MonadRandom r, Floating a)
            => r a -> r a -> (a -> a) -> (Int, Int) -> r (Layer a)
randomLayer rW rB a (m, n) = do
    w <- replicateM n $ replicateM m rW
    b <- replicateM n rB
    return $ Layer { weights = w, biases = b, activation = a }

randomNet :: (MonadRandom r, Floating a)
          => r a -> r a -> [Int] -> [a -> a] -> r (NeuralNet a)
randomNet _ _ [] _ = return $ NeuralNet []
randomNet _ _ _ [] = return $ NeuralNet []
randomNet rW rB sizes@(_:ts) activations =
    NeuralNet <$> traverse generateLayer sizesAndFns
    where sizesAndFns = zip3 sizes ts activations
          generateLayer (m, n, a) = randomLayer rW rB a (m, n)

-- Prediction

layerPredict :: Floating a => [a] -> Layer a -> [a]
layerPredict inputs layer = activation layer <$>
    (zipWith (+) (biases layer) $
        sum . zipWith (*) inputs <$> weights layer)

networkPredict :: Floating a => NeuralNet a -> [a] -> [a]
networkPredict net inputs = foldl' layerPredict inputs $ layers net
