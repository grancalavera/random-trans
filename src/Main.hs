{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                           hiding ( (!!) )
import           System.Random                            ( getStdGen )
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
                                                          , liftRandT
                                                          )
import           Control.Lens                             ( (#) )
import           Control.Monad.Trans                      ( liftIO )
import           Control.Monad.Trans.Except

import           Data.Validation

-- Dropped the deprecated Control.Monad.Trans.Either.
-- Use Control.Monad.Trans.Except from transformers and/or transformers-compat instead.

newtype Range a = Range { unRange :: (a ,a) }

data Error = FromGreaterThanTo
           | NotAnInteger String
           | NotEven Int
           | NotOdd Int
           | InvalidFromEvenToOddRange
           deriving Show

data Parity = Even | Odd deriving Show

main :: IO ()
main = do
  let input = [1, 2, 3, 4, 5]
  putStrLn $ "Original input:  " <> show input
  result <- evalRandIO (shuffle input)
  putStrLn $ "Shuffled result: " <> show result

shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return $ x' : xs'

choose :: (RandomGen g) => NonEmpty a -> Rand g a
choose xs = (xs !!) <$> getRandomR (0, length xs - 1)

asEither :: Either e a -> Either e a
asEither = id

mkEvenOddPair :: String -> String -> Validation [Error] (Int, Int)
mkEvenOddPair evenCandidate oddCandidate =
  (,)
    <$> mkIntWithParity Even evenCandidate
    <*> mkIntWithParity Odd  oddCandidate

mkInteger :: (Int -> Validation [Error] ()) -> String -> Validation [Error] Int
mkInteger validateParity s = fromEither $ do
  int <- parseInt s
  toEither $ validateParity int
  _Success # int

mkIntWithParity :: Parity -> String -> Validation [Error] Int
mkIntWithParity parity candidate = fromEither $ do
  int <- parseInt candidate
  toEither $ case parity of
    Odd  -> validate [NotOdd int] odd int
    Even -> validate [NotEven int] even int

mkRange :: (Validate f, Ord a) => (a, a) -> f [Error] (Range a)
mkRange (from, to) = if from <= to
  then _Success # Range (from, to)
  else _Failure # [FromGreaterThanTo]

parseInt :: Validate f => String -> f [Error] Int
parseInt s = maybe (_Failure # [NotAnInteger s]) (_Success #) (readMaybe s)
