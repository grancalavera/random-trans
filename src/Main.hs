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
import           Control.Monad.IO.Class                   ( MonadIO )
import           Control.Monad.Trans                      ( liftIO )
import           Control.Monad.Except                     ( ExceptT
                                                          , liftEither
                                                          , throwError
                                                          )

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

-- I want to create this pair to test a validation with more than 1 error
-- mkEvenOddPair :: String -> String -> Validation [Error] (Int, Int)
-- mkEvenOddPair evenCandidate oddCandidate =
--   (,)
--     <$> mkIntWithParity Even evenCandidate
--     <*> mkIntWithParity Odd  oddCandidate

mkRange :: (Validate f, Ord a) => (a, a) -> f [Error] (Range a)
mkRange (from, to) = if from <= to
  then _Success # Range (from, to)
  else _Failure # [FromGreaterThanTo]

mkIntWithParity :: Validate f => Parity -> String -> f [Error] Int
mkIntWithParity parity candidate = case validation of
  Right x     -> _Success # x
  Left  error -> _Failure # error
 where
  validation :: Either [Error] Int
  validation = do
    int <- parseInt candidate
    validateParity parity int
    return int

validateParity :: Validate f => Parity -> Int -> f [Error] ()
validateParity parity candidate =
  let (error, predicate) = case parity of
        Odd  -> ([NotOdd candidate], odd)
        Even -> ([NotEven candidate], even)
  in  if predicate candidate then _Success # () else _Failure # error

parseInt :: Validate f => String -> f [Error] Int
parseInt s = maybe (_Failure # [NotAnInteger s]) (_Success #) (readMaybe s)
