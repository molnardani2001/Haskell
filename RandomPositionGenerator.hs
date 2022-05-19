module MineSweeper.RandomPositionGenerator where

import System.Random ( mkStdGen, Random(..) )

instance (Random a, Random b) => Random (a,b) where
    random g = ((x, y), g2)
        where
            (x, g1) = random g
            (y, g2) = random g1
    randomR ((a, b), (c, d)) g = ((x, y), g2)
        where
            (x, g1) = randomR (a, c) g
            (y, g2) = randomR (b, d) g1

type Seed = Int

generatePosition :: Seed -> (Int, Int)
generatePosition = fst . random . mkStdGen

generatePositions :: Seed -> [(Int, Int)]
generatePositions = randoms . mkStdGen

generatePositionR :: ((Int, Int), (Int, Int)) -> Seed -> (Int, Int)
generatePositionR a = fst . randomR a . mkStdGen

generatePositionRs :: ((Int, Int), (Int, Int)) -> Seed -> [(Int, Int)]
generatePositionRs a = randomRs a . mkStdGen