module Main where

import           Prelude                           hiding ( (!!) )
import           Data.List.NonEmpty                       ( NonEmpty
                                                          , (!!)
                                                          , fromList
                                                          )

import           Control.Monad.Random.Strict              ( RandomGen
                                                          , Rand
                                                          , evalRandIO
                                                          , getRandomR
                                                          )

main :: IO ()
main = do
  print =<< evalRandIO (shuffle ([] :: [Int]))
  print =<< evalRandIO (shuffle [1])
  print =<< evalRandIO (shuffle [1 .. 10])

shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return $ x' : xs'

choose :: (RandomGen g) => NonEmpty a -> Rand g a
choose xs = (xs !!) <$> getRandomR (0, length xs - 1)
