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
                                                          , getRandomR
                                                          , evalRand
                                                          )
import           Control.Lens                             ( (#) )
import           Control.Monad                            ( void )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Except                     ( ExceptT
                                                          , liftEither
                                                          , runExceptT
                                                          )

import           Data.Validation                          ( Validate
                                                          , _Failure
                                                          , _Success
                                                          )
import           Options.Applicative                      ( Parser
                                                          , flag
                                                          , long
                                                          , short
                                                          , help
                                                          , execParser
                                                          , helper
                                                          , info
                                                          , fullDesc
                                                          , progDesc
                                                          )

-- http://dev.stephendiehl.com/hask/#mtl-transformers

-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- liftIO :: MonadIO m => IO a -> m a

-- class MonadTrans t where
--     lift :: Monad m => m a -> t m a

-- class (Monad m) => MonadIO m where
--     liftIO :: IO a -> m a

-- instance MonadIO IO where
--     liftIO = id

-- In terms of generality the mtl library is the most common general interface for these monads,
-- which itself depends on the transformers library which generalizes the "basic" monads described
-- above into transformers.

data Error = FromGreaterThanTo
           | NotAnInteger String
           | NotEven Int
           | NotOdd Int
           | InvalidFromEvenToOddRange
           deriving Show

data ParityOrder = OddEven | EvenOdd deriving (Show)

data Options = Options
  { parityOrder :: ParityOrder } deriving Show

options :: Parser Options
options = Options <$> flag
  OddEven
  EvenOdd
  (long "invert-parity-order" <> short 'i' <> help
    "Invert parity order: from odd-even to even-odd"
  )


data Parity = Even | Odd deriving Show

type Program m a = ExceptT [Error] m a



main :: IO ()
main = do
  result <- execParser opts
  print result
  void $ runExceptT program >>= print
 where
  opts = info
    (helper <*> options)
    (fullDesc <> progDesc "Validation and Random in the same program")

program :: MonadIO m => Program m [Int]
program = do

  liftIO $ putStrLn "Enter an even integer"
  evenCandidate <- liftIO getLine

  liftIO $ putStrLn ("Enter an odd integer greater than " <> evenCandidate)
  oddCandidate <- liftIO getLine

  (from, to)   <- liftEither $ mkEvenOddPair evenCandidate oddCandidate
  liftEither $ validateRange (from, to)

  let input = [from .. to]
  liftIO $ putStrLn ("Input values: " <> show input)

  gen <- liftIO getStdGen
  let result = evalRand (shuffle input) gen
  liftIO $ putStrLn ("Shuffled values: " <> show result)
  return result




shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return $ x' : xs'

choose :: (RandomGen g) => NonEmpty a -> Rand g a
choose xs = (xs !!) <$> getRandomR (0, length xs - 1)

-- I want to create this pair to test a validation with more than 1 error
mkEvenOddPair
  :: (Validate f, Applicative (f [Error]))
  => String
  -> String
  -> f [Error] (Int, Int)
mkEvenOddPair evenCandidate oddCandidate =
  (,)
    <$> mkIntWithParity Even evenCandidate
    <*> mkIntWithParity Odd  oddCandidate

validateRange :: (Validate f, Ord a) => (a, a) -> f [Error] ()
validateRange (from, to) =
  if from < to then _Success # () else _Failure # [FromGreaterThanTo]

mkIntWithParity :: Validate f => Parity -> String -> f [Error] Int
mkIntWithParity parity candidate = either (_Failure #) (_Success #) $ do
  int <- parseInt candidate
  validateParity parity int
  return int

validateParity :: Validate f => Parity -> Int -> f [Error] ()
validateParity parity candidate =
  let (err, predicate) = case parity of
        Odd  -> ([NotOdd candidate], odd)
        Even -> ([NotEven candidate], even)
  in  if predicate candidate then _Success # () else _Failure # err

parseInt :: Validate f => String -> f [Error] Int
parseInt s = maybe (_Failure # [NotAnInteger s]) (_Success #) (readMaybe s)
