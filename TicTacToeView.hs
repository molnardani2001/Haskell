module TicTacToeView where

import TicTacToe
import Data.List
import Text.Read

instance Show Player where
    show PlayerX = "X"
    show PlayerO = "O"

showMaybePlayer :: Maybe Player -> String
showMaybePlayer Nothing  = " "
showMaybePlayer (Just p) = show p

instance Show State where
    show Running = "Next player is: "
    show (GameOver p)
        | Just r <- p = "The winner is: " ++ show r 
        | otherwise   = "It's a tie!"

instance Show Board where
    show (Board b) = intercalate "\n" $ map ((\x -> '|':intercalate "|" x ++ "|") . map showMaybePlayer) b

instance Show Game where
    show (Game b p s@Running) = intercalate "\n" [show b, show s ++ show p]
    show (Game b _ s) = intercalate "\n" [show b, show s]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn e [] = []
splitOn e l = splitOn1 e l
    where
        splitOn1 e [] = [[]]
        splitOn1 e (x : xs)
            | e == x = [] : splitOn1 e xs
            | otherwise = (x : y) : ys
            where
                (y:ys) = splitOn1 e xs

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

main :: IO ()
main = do
    putStrLn $ show initGame
    iterateUntilM isGameOver (\game -> do
        move <- getLine
        let step = (map readMaybe $ splitOn ' ' move) :: [Maybe Int]
        case step of
            [Just a, Just b] -> do 
                    let next = playerTurn game (a,b)
                    putStrLn $ show next
                    return next
            _ -> do
                putStrLn "Invalid input format!"
                putStrLn $ show game
                return game
        ) initGame
    return ()