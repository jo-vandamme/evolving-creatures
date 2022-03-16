module GATest where

import           Control.Monad.Random
import           GA
import           GAOperators

newtype CharDNA = CharDNA String deriving (Show)

target :: [Char]
target = "Hello, world!"

instance DNA CharDNA where
    fitness (CharDNA g) = return $ fromIntegral . sum . map fromEnum $ zipWith (==) target g
    crossover (CharDNA a) (CharDNA b) = CharDNA <$> uniformCrossover a b
    mutate (CharDNA g) = CharDNA <$> mutateSingle g (const $ getRandomR (' ', 'z'))
    generate = CharDNA . take (length target) <$> getRandomRs (' ', 'z')
    reset = return

initPopulation :: MonadRandom r => r (Population CharDNA)
initPopulation = randomPopulation $ Parameters
    { size           = 1024
    , pCrossover     = 0.8
    , pMutation      = 0.03
    , elitism        = 0.1
    , tournamentSize = 6
    }

showStats :: Int -> Population CharDNA -> IO ()
showStats gen (Population _ pop) = do
    let best = head pop
    fit <- fitness best
    putStrLn $ concat [ show gen, " - fitness: "
                      , show fit
                      , " - ", show best
                      ]

testGA :: IO ()
testGA = initPopulation >>= runGA 20 showStats
                        >>= \(_, best) ->
                                putStrLn $ "Done: " ++ show best

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM n action a
    | n <= 1    = return a
    | otherwise = action a >>= repeatM (n - 1) action

evolveN :: Int -> IO CharDNA
evolveN n = evalRandIO $ initPopulation
            >>= repeatM n evolve
            >>= (\(Population _ (x:_)) -> return x)
