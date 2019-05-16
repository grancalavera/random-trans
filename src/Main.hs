{-# LANGUAGE ExplicitForAll #-}

module Main where

import           Prelude                           hiding ( (!!) )
import           Text.Read                                ( readMaybe )
import           Data.Maybe                               ( maybe )
import           Data.List.NonEmpty                       ( NonEmpty
                                                          , (!!)
                                                          , fromList
                                                          )
import           Control.Monad.Random.Strict              ( RandomGen
                                                          , Rand
                                                          , evalRandIO
                                                          , getRandomR
                                                          )
import           Control.Lens                             ( (#) )
import           Data.Validation

newtype Range a = Range { unRange :: (a ,a) }

data Error = FromGreaterThanTo
           | NotAnInteger String
           | NotEven Int
           | NotOdd Int
           | InvalidFromEvenToOddRange
           deriving Show

main :: IO ()
main = do
  putStrLn "Enter an even integer:"
  fromS <- getLine
  putStrLn
    "Enter an odd integer GREATER than the even integer you just entered:"
  toS <- getLine
  putStrLn $ "from: " <> fromS <> " to: " <> toS

shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return $ x' : xs'

choose :: (RandomGen g) => NonEmpty a -> Rand g a
choose xs = (xs !!) <$> getRandomR (0, length xs - 1)

mkEvenOddPair :: String -> String -> Validation [Error] (Int, Int)
mkEvenOddPair evenS oddS =
  (,) <$> mkInteger validateEven evenS <*> mkInteger validateOdd oddS

mkInteger :: (Int -> Validation [Error] ()) -> String -> Validation [Error] Int
mkInteger validateParity s = fromEither $ do
  int <- parseInt s
  toEither $ validateParity int
  _Success # int

parseInt :: Validate f => String -> f [Error] Int
parseInt s = maybe (_Failure # [NotAnInteger s]) (_Success #) (readMaybe s)

validateOdd, validateEven :: Validate f => Int -> f [Error] ()
validateOdd int = validateInt (NotOdd int) odd int
validateEven int = validateInt (NotEven int) even int

validateInt :: Validate f => Error -> (Int -> Bool) -> Int -> f [Error] ()
validateInt e isValid x | isValid x = _Success # ()
                        | otherwise = _Failure # [e]

mkRange :: Ord a => (a, a) -> Validation [Error] (Range a)
mkRange r = Range <$> validate [FromGreaterThanTo] (uncurry (<)) r
