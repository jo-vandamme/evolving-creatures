module GAOperators where

import           Control.Monad.Random

uniformCrossover :: MonadRandom r => [a] -> [a] -> r [a]
uniformCrossover a b =
    zipWith3 (\x y r -> if r then x else y)
        <$> pure a
        <*> pure b
        <*> getRandoms

blendCrossover :: (MonadRandom r, Random a, Floating a) => [a] -> [a] -> r [a]
blendCrossover a b =
    zipWith3 (\x y r -> r * x + (1 - r) * y)
        <$> pure a
        <*> pure b
        <*> getRandoms

singlePointCrossover :: MonadRandom r => [a] -> [a] -> r [a]
singlePointCrossover a b =
    (++) <$> flip take a
         <*> flip drop b
         <$> pivot
         where pivot = getRandomR (0, length a - 1)

mutateSingle :: MonadRandom r => [a] -> (a -> r a) -> r [a]
mutateSingle xs f = do
    idx <- getRandomR (0, length xs - 1)
    a <- f (xs !! idx)
    return $ modifyNth idx (const a) xs

mutateN :: MonadRandom r => Int -> [a] -> (a -> r a) -> r [a]
mutateN 0 xs _ = return xs
mutateN n xs f = do
    ys <- mutateSingle xs f
    mutateN (n-1) ys f

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
    | n == 0 = f x : xs
    | otherwise = x : modifyNth (n-1) f xs
