module Simulation where

import           Food
import qualified GA
import           Math
import           NeuralNet
import           Organism
import           OrganismDNA
import           Parameters

import           Control.Monad        (replicateM)
import           Control.Monad.Random (MonadRandom)
import           Data.Bifunctor       (bimap)
import           Data.Fixed           (mod')
import           Data.List            (minimumBy)

data Stats = Stats
    { meanFps    :: Float
    , generation :: Int
    , bestScore  :: Float
    , meanScore  :: Float
    , step       :: Int
    , duration   :: Float
    } deriving (Show)

data Simulation = Simulation
    { organisms     :: GA.Population OrganismDNA
    , foodParticles :: [Food]
    , stats         :: Stats
    } deriving (Show)

randomSimulation :: MonadRandom r => r Simulation
randomSimulation = do
    let gaParameters = GA.Parameters { GA.size = popSize
                                     , GA.pCrossover = crossover
                                     , GA.pMutation = mutation
                                     , GA.elitism = elitism
                                     , GA.tournamentSize = tournamentSize
                                     }
    orgs <- GA.randomPopulation gaParameters
    food <- replicateM numFood randomFood
    let s = Stats { meanFps = 0
                  , generation = 0
                  , bestScore = 0
                  , meanScore = 0
                  , duration = 0
                  , step = 0
                  }
    return $ Simulation { organisms = orgs
                        , foodParticles = food
                        , stats = s
                        }

updateOrg :: Float -> [Food] -> Organism -> Organism
updateOrg dt food o =
    let (nnDv, nnDr, points) = useBrain food o
        dr = nnDr * wMax * dt
        r  = mod' (heading o + dr) 360
        dv = nnDv * aMax * dt
        v  = max 0 . min vMax . (+dv) . speed $ o
        dx = v * dt * cos (radians r)
        dy = v * dt * sin (radians r)
     in o { orgPos = bimap (+ dx) (+ dy) (orgPos o)
          , heading = r
          , speed = v
          , health = health o + points
          }

updateFood :: MonadRandom r => [OrganismDNA] -> Food -> r Food
updateFood [] f = return f
updateFood ((OrganismDNA o):os) f =
    if d < eatRadius
       then randomFood
       else updateFood os f
    where d = vdist (orgPos o) (foodPos f)

useBrain :: [Food] -> Organism -> (Float, Float, Float)
useBrain food o =
    case findClosestFood food o of
        Just (f, dmin) -> go f dmin
        Nothing        -> (0, 0, 0)
    where go f d =
            let angle = normalizedAngleToFood (foodPos f) (orgPos o) (heading o)
                out = networkPredict (brain o) [angle]
                dr = head out
                dv = head . tail $ out
                points = if d < eatRadius then energy f else 0
            in (dr, dv, points)

findClosestFood :: [Food] -> Organism -> Maybe (Food, Float)
findClosestFood [] _ = Nothing
findClosestFood fds@(f:_) o = Just $ foldr go (f, vdist (foodPos f) (orgPos o)) fds
    where go f' (fmin, dmin) = if d < dmin then (f', d) else (fmin, dmin)
            where d = vdist (foodPos f') (orgPos o)

normalizedAngleToFood :: Vec2 -> Vec2 -> Float -> Float
normalizedAngleToFood fPos oPos oDir = theta' / 180
    where (x, y) = vdiff fPos oPos
          theta = degrees (atan2 y x) - oDir
          theta' = if abs theta > 180 then theta + 360 else theta

updateStats :: Bool -> Float -> Simulation -> Stats
updateStats newGen dt sim = stats' { meanFps = meanFps'
                                   , generation = generation'
                                   , bestScore = bestScore'
                                   , meanScore = meanScore'
                                   , step = step'
                                   , duration = duration'
                                   }
    where stats' = stats sim
          alpha = 0.05
          currentFps = 1 / dt
          meanFps' = alpha * currentFps + (1 - alpha) * meanFps stats'
          generation' = if newGen then generation stats' + 1 else generation stats'
          bestScore' = if newGen then 0 else getBestScore . organisms $ sim
          meanScore' = if newGen then 0 else getMeanScore . organisms $ sim
          step' = if newGen then 0 else step stats' + 1
          duration' = if newGen then 0 else duration stats' + dt

getMeanScore :: GA.Population OrganismDNA -> Float
getMeanScore (GA.Population _ []) = 0
getMeanScore (GA.Population _ os) = getMean os
    where getMean = (/) <$> sum . map getScore <*> fromIntegral . length

getBestScore :: GA.Population OrganismDNA -> Float
getBestScore (GA.Population _ []) = 0
getBestScore (GA.Population _ os) = getScore (minimumBy comparePoints os)
    where comparePoints a b = compare (getScore b) (getScore a)

getScore :: OrganismDNA -> Float
getScore (OrganismDNA o) = health o

stepSimulation :: Float -> Simulation -> IO Simulation
stepSimulation dt sim = do
    if (duration . stats) sim >= simDuration
    then do
        o <- GA.evolve (organisms sim)
        food <- replicateM numFood randomFood
        let sim' = sim { organisms = o, foodParticles = food }
        return $ sim' { stats = updateStats True dt sim' }
    else do
        let food = foodParticles sim
            (GA.Population params orgPop) = organisms sim
            updateO (OrganismDNA o) = OrganismDNA $ updateOrg dt food o
            updatedOrgPop = GA.Population params $ map updateO orgPop
        updatedFood <- mapM (updateFood orgPop) food
        let sim' = sim { organisms = updatedOrgPop
                       , foodParticles = updatedFood
                       }
        return $ sim' { stats = updateStats False dt sim' }
