module MineSweeper.FieldNumber (FieldNumber, fieldNumber, getFieldNumber) where

import Data.Function (on)
import Data.List (minimumBy)

newtype FieldNumber = FieldNumber {getFieldNumber :: Int} deriving (Eq, Ord)

instance Bounded FieldNumber where
    minBound = FieldNumber 0
    maxBound = FieldNumber 8
    
instance Enum FieldNumber where
    succ (FieldNumber int) = fieldNumber $ succ int
    pred (FieldNumber int) = fieldNumber $ pred int
    toEnum = fieldNumber
    fromEnum (FieldNumber int) = int
    enumFrom (FieldNumber start) = map fieldNumber $ enumFromThenTo start (succ start) 8
    enumFromThen (FieldNumber start) (FieldNumber next) = map fieldNumber $ enumFromThenTo start next $ fst $ minimumBy (compare `on` snd) [(0, next), (8, start)]
    enumFromTo (FieldNumber start) (FieldNumber end) = map fieldNumber $ enumFromThenTo start (succ start) end
    enumFromThenTo (FieldNumber start) (FieldNumber next) (FieldNumber end) = map fieldNumber $ enumFromThenTo start next (min 8 end)

instance Show FieldNumber where
    showsPrec prec (FieldNumber integer) = showsPrec prec integer

instance Read FieldNumber where
    readsPrec prec str = map (first fromInteger) (readsPrec prec str) where
        first :: (val -> val') -> (val,other) -> (val',other)
        first f (val, other) = (f val, other)

instance Num FieldNumber where
    FieldNumber int1 + FieldNumber int2 
        | 0 <= x && x <= 8 = FieldNumber x
        | otherwise = errorWithoutStackTrace "FieldNumber.(+): The sum of the two numbers exceeds 8!"
        where x = int1 + int2
    FieldNumber int1 - FieldNumber int2
        | 0 <= x && x <= 8 = FieldNumber x
        | otherwise = errorWithoutStackTrace "FieldNumber.(-): The difference of the two numbers subceeds 0!"
        where x = int1 - int2
    FieldNumber int1 * FieldNumber int2
        | 0 <= x && x <= 8 = FieldNumber x
        | otherwise = errorWithoutStackTrace "FieldNumber.(*): The product of the two numbers exceeds 8!"
        where x = int1 * int2
    abs = id
    signum x@(FieldNumber 0) = x
    signum _ = FieldNumber 1
    fromInteger = fieldNumber . fromIntegral

instance Real FieldNumber where
    toRational = toRational . toInteger

fieldNumber :: Int -> FieldNumber
fieldNumber i
    | i `elem` [0..8] = FieldNumber i
    | otherwise = errorWithoutStackTrace "FieldNumber: FieldNumber can only be from [0..8]!"

instance Integral FieldNumber where
    quotRem (FieldNumber int1) (FieldNumber int2) = let (quot,rem) = quotRem int1 int2 in (fieldNumber quot, fieldNumber rem)
    divMod (FieldNumber int1) (FieldNumber int2) = let (quot,rem) = divMod int1 int2 in (fieldNumber quot, fieldNumber rem)
    toInteger (FieldNumber int) = toInteger int