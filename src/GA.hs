{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module GA where

import Control.Applicative (liftA2)
import Control.Monad.Random
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.ListM (maximumByM, sortByM)

class DNA a where
    fitness :: MonadRandom r  => a -> r Float
    crossover :: MonadRandom r => a -> a -> r a
    mutate :: MonadRandom r => a -> r a
    generate :: MonadRandom r => r a

data Parameters = Parameters
    { size           :: Int
    , pCrossover     :: Float
    , pMutation      :: Float
    , elitism        :: Float
    , maxGenerations :: Int
    , tournamentSize :: Int
    } deriving (Show)

data Population a =
    Population Parameters [a]
    deriving (Show)

randomPopulation :: (DNA a, MonadRandom r) => Parameters -> r (Population a)
randomPopulation =
    liftA2 Population <$> pure <*> flip replicateM generate . size

comparingM :: (Ord a, Applicative m) => (b -> m a) -> b -> b -> m Ordering
comparingM p x y = compare <$> p x <*> p y

tournamentSelection :: (DNA a, MonadRandom r) => Population a -> r a
tournamentSelection (Population info pop) =
    maximumByM (comparingM fitness) =<< map (pop !!) <$>
    replicateM (tournamentSize info) (getRandomR (0, size info - 1))

twoM :: Monad m => m a -> m (a, a)
twoM = liftM (\[x, y] -> (x, y)) . replicateM 2

selectParents :: (DNA a, MonadRandom r) => Population a -> r (a, a)
selectParents = twoM . tournamentSelection

generateOffspring :: (DNA a, MonadRandom r) => Population a -> Int -> r a
generateOffspring p@(Population info pop) idx =
    twoM getRandom >>= go
    where go (r1, r2)
            | r1 <= pCross = selectParents p >>= uncurry crossover
                                             >>= mutateChild r2
            | otherwise    = addMutation r2
          mutateChild r child
            | r <= pMut   = mutate child
            | otherwise   = return child
          addMutation r
            | r <= pMut   = mutate =<< selected
            | otherwise   = selected
            where selected = (pop !!) <$> getRandomR (idx, popSize - 1)
          pCross  = pCrossover info
          pMut    = pMutation info
          popSize = size info

evolve :: (DNA a, MonadRandom r) => Population a -> r (Population a)
evolve p@(Population info pop) =
    Population <$> pure info <*>
        (sortByM (flip $ comparingM fitness) =<< (elite ++) <$> offsprings)
        where elite = take nOld pop
              offsprings = replicateM (popSize - nOld) (generateOffspring p nOld)
              nOld  = round (fromIntegral popSize * elitism info)
              popSize = size info

runGA :: DNA a
      => (Int -> Population a -> IO ())
      -> Population a
      -> IO (Int, Maybe a)
runGA statAction pop@(Population info _) =
    result <$> iterateUntilM done step (pop, 0)
    where step (p, gen) = do
            statAction gen p
            (,) <$> evolve p <*> pure (gen + 1)
          done ((Population _ _), gen) = gen == maxGen
          result ((Population _ (g:_)), gen) = (gen, Just g)
          result ((Population _ []), gen) = (gen, Nothing)
          maxGen = maxGenerations info
