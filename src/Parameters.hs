module Parameters where

simDuration, popSize, numFood, width, height, tournamentSize :: Num a => a
vMax, aMax, wMax, eatRadius, crossover, mutation, elitism :: Floating a => a
width = 600 -- px
height = 600 -- px
vMax = 200 -- px/s
aMax = 100 -- px/s^2
wMax = 360 * 10 -- deg
popSize = 300
numFood = 150
eatRadius = 5 -- px
simDuration = 10 -- s
crossover = 0.8 -- 0-1
mutation = 0.03 -- 0-1
elitism = 0.1 -- 0-1
tournamentSize = 10
