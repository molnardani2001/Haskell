module TicTacToe where
import Data.List

data Player = PlayerX | PlayerO
   deriving (Eq)

type Cell = Maybe Player

data State =  Running | GameOver (Maybe Player)

newtype Board = Board [[Cell]]

data Game = Game Board Player State

gameBoard :: Game -> Board
gameBoard (Game b _ _) = b

gamePlayer :: Game -> Player
gamePlayer (Game _ p _) = p

gameState :: Game -> State
gameState (Game _ _ s) = s

size :: Int
size = 3

initGame :: Game
initGame = Game (Board (replicate size (replicate size Nothing))) PlayerX Running

switchPlayer :: Game -> Game
switchPlayer (Game b p s)
    | p == PlayerX = Game b PlayerO s
    | otherwise = Game b PlayerX s

countEmptyCells :: Board -> Int
countEmptyCells (Board list) = sum $ map genericLength helper where
    helper = map (filter (==Nothing)) list

full :: [Cell] -> Maybe Player
full [] = Nothing
full (x:xs)
    | all (==x) xs = x
    | otherwise = Nothing

isEmpty :: [Cell] -> Maybe Player
isEmpty list
   | filter (/=Nothing) list == [] = Nothing
   | otherwise = head $ filter (/=Nothing) list

winner :: Board -> Maybe Player
winner (Board list) = isEmpty (horizontal : vertical : [fDiag,bDiag]) where
   horizontal
      | all ((==Nothing) . full) list = Nothing
      | otherwise = head (filter (/=Nothing) (map full list))
   vertical
      | all ((==Nothing) . full) (transpose list) = Nothing
      | otherwise = head (filter (/=Nothing) (map full (transpose list)))
   fDiag
      | full (zipWith (!!) list [0..]) /= Nothing = head (zipWith (!!) list [0..])
      | otherwise = Nothing
   bDiag
      | full (zipWith (!!) (reverse list) [0..]) /= Nothing = head (zipWith (!!) (reverse list) [0..])
      | otherwise = Nothing

checkGameOver :: Game -> Game
checkGameOver (Game b p s)
   | winner b /= Nothing = Game b p (GameOver (winner b))
   | countEmptyCells b == 0 = Game b p (GameOver Nothing)
   | otherwise = Game b p s


replaceAt :: Int -> a -> [a] -> [a]
replaceAt num what = helper num where
   helper 0 (_:xs) = what : xs
   helper n (x:xs) = x : helper (n-1) xs
   helper _ [] = []

playerTurn :: Game -> (Int,Int) -> Game
playerTurn (Game b p (GameOver _)) _ = Game b p (GameOver (winner b))
playerTurn (Game (Board b) p s) (x,y) 
   | x >=0 && x < size && y >=0 && y < size && (((b !! x) !! y) == Nothing) = checkGameOver $ switchPlayer (Game (Board (replaceAt x (replaceAt y (Just p) (b !! x)) b)) p s)
   | otherwise = Game (Board b) p s

isGameOver :: Game -> Bool
isGameOver (Game _ _ Running) = False
isGameOver _ = True

