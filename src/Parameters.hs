module Parameters where

simDuration, popSize, numFood, width, height, numSteps, tournamentSize :: Num a => a
vMax, aMax, wMax, eatRadius, crossover, mutation, elitism :: Floating a => a
width = 600 -- m
height = 600 -- m
vMax = 100 -- m/s
aMax = 20 -- m/s^2
wMax = 360 * 3 -- deg
popSize = 200
numFood = 150
eatRadius = 5 -- m
numSteps = 1000
simDuration = 10 -- s
crossover = 0.8 -- 0-1
mutation = 0.03 -- 0-1
elitism = 0.1 -- 0-1
tournamentSize = 10

