module Organism where

import           Math
import           NeuralNet
import           Parameters

import           Control.Monad.Random (MonadRandom, getRandomR)

activations :: Floating a => [a -> a]
activations = repeat tanh

brainTopo :: [Int]
brainTopo = [1, 5, 2]

data Organism = Organism
    { orgPos  :: Vec2
    , heading :: Float
    , speed   :: Float
    , accel   :: Float
    , brain   :: NeuralNet Float
    , health  :: Float
    } deriving (Show)

randomOrganism :: MonadRandom r => r Organism
randomOrganism = do
    x <- getRandomR (0, width)
    y <- getRandomR (0, height)
    d <- getRandomR (0, 360)
    v <- getRandomR (0, vMax)
    a <- getRandomR (-aMax, aMax)
    b <- randomNet uniform uniform brainTopo activations
    return $ Organism { orgPos = (x, y)
                      , heading = d
                      , speed = v
                      , accel = a
                      , brain = b
                      , health = 0
                      }

