{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- The ConstraintKinds extension allows for using MonadReader providing the reader
-- type but not the underlying monad; coupled with FlexibleContexts, AppOptions can
-- use parametric polymorphism
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( ReaderT
                                                          , MonadReader
                                                          , asks
                                                          , runReaderT
                                                          )
import           Control.Monad.Except                     ( ExceptT
                                                          , MonadError
                                                          , liftEither
                                                          , runExceptT
                                                          )

import           Data.Validation                          ( Validate
                                                          , _Failure
                                                          , _Success
                                                          , toEither
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
type Program m a = ExceptT [AppError] m a

data Parity = Even | Odd deriving Show

data AppError = FromGreaterThanTo
              | NotAnInteger String
              | NotEven Int
              | NotOdd Int
              | InvalidFromEvenToOddRange
           deriving Show

data ParityOrder = OddEven | EvenOdd deriving (Show)

newtype Options = Options
  { parityOrder :: ParityOrder } deriving Show


type AppConfig = MonadReader Options

newtype App a = ProgramStack {
  runApp :: ReaderT Options (ExceptT [AppError] IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError [AppError])

main :: IO ()
main = do
  options <- execParser opts
  result  <- runExceptT (runReaderT (runApp run) options)
  either renderError renderResult result
 where
  opts = info
    (helper <*> parseOptions)
    (fullDesc <> progDesc "Validation and Random in the same program")

run :: App [Int]
run = getUserInput >>= mkRangeFromInput >>= shuffleRange

parseOptions :: Parser Options
parseOptions = Options <$> flag
  OddEven
  EvenOdd
  (long "invert-parity-order" <> short 'i' <> help
    "Invert parity order: from odd-even (default) to even-odd (inverted)"
  )

renderError :: [AppError] -> IO ()
renderError = foldl (\printPrevious this -> printPrevious >> print this)
                    (putStrLn "Program failed!")

renderResult :: [Int] -> IO ()
renderResult = putStrLn . ("Shuffled values: " <>) . show

getUserInput :: (AppConfig m, MonadIO m) => m (String, String)
getUserInput = do
  order <- asks parityOrder
  case order of
    OddEven -> prompt "odd" "even"
    EvenOdd -> prompt "even" "odd"
 where
  prompt q1 q2 = do
    c1 <- getCandidate $ "Enter an " <> q1 <> " integer"
    c2 <- getCandidate $ "Enter an " <> q2 <> " integer greater than " <> c1
    return (c1, c2)
  getCandidate q = do
    liftIO $ putStrLn q
    liftIO getLine

mkRangeFromInput
  :: (AppConfig m, MonadError [AppError] m) => (String, String) -> m [Int]
mkRangeFromInput candidates = do
  pOrder     <- asks parityOrder
  (from, to) <- liftEither . toEither $ mkPairWithParityOrder pOrder candidates
  liftEither $ validateRange (from, to)
  return [from .. to]

shuffleRange :: (AppConfig m, MonadIO m) => [Int] -> m [Int]
shuffleRange range = do
  gen <- liftIO getStdGen
  return $ evalRand (shuffle range) gen

shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return $ x' : xs'

choose :: (RandomGen g) => NonEmpty a -> Rand g a
choose xs = (xs !!) <$> getRandomR (0, length xs - 1)

-- Allows to test a validation with more than 1 error
-- for instance sending two strings that will not
-- validate as Int
mkPairWithParityOrder
  :: (Validate f, Applicative (f [AppError]))
  => ParityOrder
  -> (String, String)
  -> f [AppError] (Int, Int)
mkPairWithParityOrder pOrder = case pOrder of
  OddEven -> mkPair Odd Even
  EvenOdd -> mkPair Even Odd
 where
  mkPair p1 p2 (c1, c2) =
    (,) <$> mkIntWithParity p1 c1 <*> mkIntWithParity p2 c2

validateRange :: (Validate f, Ord a) => (a, a) -> f [AppError] ()
validateRange (from, to) =
  if from < to then _Success # () else _Failure # [FromGreaterThanTo]

mkIntWithParity :: Validate f => Parity -> String -> f [AppError] Int
mkIntWithParity parity candidate = either (_Failure #) (_Success #) $ do
  int <- parseInt candidate
  validateParity parity int
  return int

validateParity :: Validate f => Parity -> Int -> f [AppError] ()
validateParity parity candidate =
  let (err, predicate) = case parity of
        Odd  -> ([NotOdd candidate], odd)
        Even -> ([NotEven candidate], even)
  in  if predicate candidate then _Success # () else _Failure # err

parseInt :: Validate f => String -> f [AppError] Int
parseInt s = maybe (_Failure # [NotAnInteger s]) (_Success #) (readMaybe s)
