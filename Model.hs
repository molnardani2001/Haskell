module MineSweeper.Model where

import MineSweeper.FieldNumber
import MineSweeper.RandomPositionGenerator
import Data.List
import Data.Function

type Width = Int
type Height = Int
type NumberOfBombs = Int

data FieldType = Mine | Free FieldNumber

data Field = Closed FieldType | Opened FieldType | Flagged FieldType 

data Board = Board Width Height [[Field]]

data State = Running | GameOver Bool

data Game = Game Board State

data MoveType = Flag (Int,Int) | Open (Int,Int)

getWidth :: Board -> Width
getWidth (Board w _ _) = w

getHeight :: Board -> Height
getHeight (Board _ h _) = h

getField :: Board -> [[Field]]
getField (Board _ _ b) = b

gameBoard :: Game -> Board
gameBoard (Game b _) = b

gameState :: Game -> State
gameState (Game _ s) = s

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) (x:_) 0 = Just x
(!!?) (x:xs) y
    | y < 0 || y >= genericLength (x:xs) = Nothing
    | otherwise = (!!?) xs (y-1)

maybeMap :: (a -> b) -> (Maybe a -> Maybe b)
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

join :: Maybe (Maybe a) -> Maybe a
join (Nothing) = Nothing
join (Just (Nothing)) = Nothing
join (Just (Just x)) = Just x

singleMedian :: Ord a => [a] -> Maybe a
singleMedian [] = Nothing
singleMedian list
    | even size= Nothing
    | otherwise = (!!?) (sort list) (size `div` 2) where
        size = length list

sortedElem :: Ord a => a -> [a] -> Bool
sortedElem _ [] = False
sortedElem del list
    | null taken || last taken /= del = False
    | otherwise = True where
        taken = takeWhile (<=del) list

generateBombsPositions :: Width -> Height -> NumberOfBombs -> Seed -> [(Int,Int)]
generateBombsPositions w h nob s = ordOrd $ take n ( nub (generatePositionRs ((0,0),(h-1,w-1)) s)) where
    n
       | ((w*h) `div` 10) <= nob && (w*h `div` 2) >= nob = nob
       | w*h `div` 10 > nob = w*h `div` 10
       | otherwise = w*h `div` 2
    ordOrd list =sortBy (compare `on` fst) $ sortBy (compare `on` snd) list

neighbours :: (Int,Int) -> [[a]] -> [[a]]
neighbours (x,y) list = map (take (3+(y-1)*(fromEnum(y<=0)))) $ map (drop (y-1)) $ take (3+(x-1)*(fromEnum(x<=0))) $ drop (x-1) list

countNeighbouringBombs :: (Int, Int) -> [(Int, Int)] -> Int
countNeighbouringBombs (a,b) list = genericLength $ filter (\x -> x == (a+1,b) || x == (a-1,b) || x == (a,b-1) || x == (a,b+1) || x == (a+1,b+1) || x == (a-1,b-1) || x == (a+1,b-1) || x == (a-1,b+1)) list

mineOrNot :: (Int,Int) -> [(Int,Int)] -> Field
mineOrNot coord list
    | coord `elem` list = Closed Mine
    | otherwise = Closed (Free (fieldNumber (countNeighbouringBombs coord list)))


generateBoard :: Width -> Height -> [(Int,Int)] -> Board
generateBoard w h list = Board w h (map (\x -> map (\y -> mineOrNot y list) x) generated) where
    generated = [ [ (a,b) | b <- [0..(w-1)] ] | a <- [0..(h-1)]]

startNewGame :: Width -> Height -> NumberOfBombs -> Seed -> Game
startNewGame w h nob s = Game (generateBoard w h (generateBombsPositions w h nob s)) Running

replaceAt :: Int -> a -> [a] -> [a]
replaceAt num what = helper num where
   helper 0 (_:xs) = what : xs
   helper n (x:xs) = x : helper (n-1) xs
   helper _ [] = []

replaceAtMatrix :: (Int,Int) -> a -> [[a]] -> [[a]]
replaceAtMatrix (a,b) what list = replaceAt a (replaceAt b what (head(drop (a) list))) list

openField :: Field -> Field
openField (Closed a) = Opened a 
openField (Flagged a) = Flagged a
openField (Opened a) = Opened a

openTile :: Game -> (Int,Int) -> Game
openTile (Game (Board w h b) s) (x,y)
    | not (null sthing') && isFree field' && getFieldNumber num == countNeighbouringFlags (Board w h b) (x,y) = openNeighbouringTiles (Game (Board w h b) s) (x,y)
    | otherwise = openClosedTile (Game (Board w h b) s) (x,y) where
        isFree :: Field -> Bool
        isFree (Opened (Free 0)) = False
        isFree (Opened (Free _)) = True
        isFree _ = False
    
        sthing' = join $ maybeMap (!!? y) $ (!!?) b x
        (Just field') = sthing'

        (Opened (Free num)) = field'

        openClosedTile :: Game -> (Int,Int) -> Game
        openClosedTile (Game (Board w h bc) sc) (x,y)
            | not ( null sthing) && isZero field = openNeighbouringTiles (Game (Board w h (replaceAtMatrix (x,y) (openField field) bc)) sc) (x,y)
            | otherwise = (Game (Board w h (replaceAtMatrix (x,y) (openField field) bc)) sc) where
                isZero :: Field -> Bool
                isZero (Closed (Free 0)) = True
                isZero _ = False

                sthing = join $ maybeMap (!!? y) $ (!!?) bc x
                (Just field) = sthing

        openNeighbouringTiles :: Game -> (Int,Int) -> Game
        openNeighbouringTiles (Game (Board w h bd) st) (a,b) = foldl (openClosedTile) (Game (Board w h bd) st) [(a-1,b),(a+1,b),(a,b-1),(a,b+1),(a-1,b-1),(a+1,b+1),(a-1,b+1),(a+1,b-1)]

        countNeighbouringFlags :: Board -> (Int,Int) -> Int
        countNeighbouringFlags (Board _ _ b) (x,y) = genericLength $ filter (flaggedSth) $ concat $ neighbours (x,y) b where
            flaggedSth :: Field -> Bool
            flaggedSth (Flagged _) = True
            flaggedSth _ = False

            

flag :: Field -> Field
flag (Closed a) = Flagged a
flag (Flagged a) = Closed a
flag (Opened a) = Opened a

flagTile :: Game -> (Int,Int) -> Game
flagTile (Game (Board w h b) Running) (x,y) = (Game (Board w h (replaceAtMatrix (x,y) (flag f) b)) Running) where
    f = head $ drop y $ head $ drop x b
flagTile (Game b s) (_,_) = (Game b s)

checkOpenedBomb :: Board -> Bool
checkOpenedBomb (Board _ _ b) = any (any (openedMine))  b where 
    openedMine :: Field -> Bool
    openedMine (Opened Mine) = True
    openedMine _ = False

checkGameOver :: Game -> Game
checkGameOver (Game (Board w h b) Running)
    | null lista = Game (Board w h b) (GameOver True)
    | checkOpenedBomb (Board w h b) = Game (showAll (Board w h b)) (GameOver False)
    | otherwise = (Game (Board w h b) Running) where
        lista = concatMap (filter (closedFree)) b where
            closedFree :: Field -> Bool
            closedFree (Closed (Free _)) = True
            closedFree (Flagged (Free _)) = True
            closedFree _ = False
        showAll :: Board -> Board
        showAll (Board w h bd) = Board w h ((map (map (openAll)) bd)) where
            openAll :: Field -> Field
            openAll (Flagged Mine) = Opened Mine
            openAll (Closed Mine) = Opened Mine
            openAll (Opened Mine) = Opened Mine
            openAll f = f
checkGameOver x = x

stepGame :: Game -> MoveType -> Game
stepGame (Game b s) (Open (x,y)) = checkGameOver (openTile (Game b s) (x,y))
stepGame (Game b s) (Flag (x,y)) = checkGameOver (flagTile (Game b s) (x,y))
