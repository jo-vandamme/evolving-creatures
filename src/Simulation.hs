module Simulation where

import Parameters
import Organism
import Food
import NeuralNet
import Math

import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom)
import Data.Fixed (mod')

data Simulation = Simulation
    { organisms     :: [Organism]
    , foodParticles :: [Food]
    , step          :: Int
    } deriving (Show)

randomSimulation :: MonadRandom r => r Simulation
randomSimulation = do
    orgs <- replicateM popSize randomOrganism
    food <- replicateM numFood randomFood
    return $ Simulation { organisms = orgs
                        , foodParticles = food
                        , step = 0
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
     in o { orgPos = ((+dx) . fst . orgPos $ o, (+dy) . snd . orgPos $ o)
          , heading = r
          , speed = v
          , health = health o + points
          }

updateFood :: MonadRandom r => [Organism] -> Food -> r Food
updateFood [] f = return f
updateFood (o:os) f =
    if d < eatRadius
       then randomFood
       else updateFood os f
    where d = vdist (orgPos o) (foodPos f) 

useBrain :: [Food] -> Organism -> (Float, Float, Float)
useBrain food o =
    case findClosestFood food o of
        Just (f, dmin) -> go f dmin
        Nothing -> (0, 0, 0)
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

stepSimulation :: MonadRandom r => Float -> Simulation -> r Simulation
stepSimulation dt s = do
    let food = foodParticles s
        orgs = organisms s
        updatedOrgs = map (updateOrg dt food) orgs
    updatedFood <- mapM (updateFood orgs) food
    return $ s { organisms = updatedOrgs 
               , foodParticles = updatedFood
               , step = step s + 1
               }
