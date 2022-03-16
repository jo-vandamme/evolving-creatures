module Food where

import           Math
import           Parameters

import           Control.Monad.Random (MonadRandom, getRandomR)

data Food = Food
    { foodPos :: Vec2
    , energy  :: Float
    , eaten   :: Bool
    } deriving (Show)

randomFood :: MonadRandom r => r Food
randomFood = do
    x <- getRandomR (0, width)
    y <- getRandomR (0, height)
    return $ Food { foodPos = (x, y)
                  , energy = 1
                  , eaten = False
                  }
