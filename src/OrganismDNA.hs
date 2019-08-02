module OrganismDNA where

import Organism
import NeuralNet
import GA
import GAOperators
import Math

import Control.Monad.Random hiding (uniform)
import Data.List.Split (chunksOf)

newtype OrganismDNA = OrganismDNA Organism deriving (Show)

instance DNA OrganismDNA where
    fitness (OrganismDNA o) = return . health $ o
    crossover (OrganismDNA a) (OrganismDNA b) = OrganismDNA <$> crossOrganisms a b
    mutate (OrganismDNA o) = OrganismDNA <$> mutateOrganism o
    generate = OrganismDNA <$> randomOrganism

crossOrganisms :: MonadRandom r => Organism -> Organism -> r Organism
crossOrganisms o1 o2 = do
    let o1b = encodeBrain $ brain o1
        o2b = encodeBrain $ brain o2
    b <- blendCrossover o1b o2b
    o <- randomOrganism
    let b' = decodeBrain brainTopo activations b
    return $ o { brain = b' }

mutateOrganism :: MonadRandom r => Organism -> r Organism
mutateOrganism o = do
    let b = encodeBrain $ brain o
        mutation x = (x * 1.2*) <$> uniform
    b' <- decodeBrain brainTopo activations <$> mutateN 2 b mutation
    return $ o { brain = b' }

encodeBrain :: NeuralNet a -> [a]
encodeBrain = concat . map encodeLayer . layers

encodeLayer :: Layer a -> [a]
encodeLayer l = (biases l) ++ concat (weights l)

decodeBrain :: [Int] -> [a -> a] -> [a] -> NeuralNet a
decodeBrain [] _ _ = NeuralNet []
decodeBrain szs@(_:ts) actFns dna = NeuralNet $ go dna layerSizes actFns
    where layerSizes = zip szs ts
          go _ _ [] = []
          go [] _ _ = []
          go _ [] _ = []
          go values ((m, n):sizes) (a:as) = layer : rest
            where layer = decodeLayer (m, n) values a
                  rest  = go (drop (m * n + n) values) sizes as

decodeLayer :: (Int, Int) -> [a] -> (a -> a) -> Layer a
decodeLayer (n1, n2) values actFn = Layer weights' biases' actFn
    where biases' = take n2 values
          weights' = chunksOf n1 $ take (n1 * n2) $ drop n2 values
