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

main :: IO ()
main = do
  putStrLn "Enter an even integer:"
  fromS <- getLine
  putStrLn
    "Enter an odd integer GREATER than the even integer you just entered:"
  toS <- getLine
  putStrLn $ "from: " <> fromS <> " to: " <> toS
  -- print =<< evalRandIO (shuffle ([] :: [Int]))
  -- print =<< evalRandIO (shuffle [1])
  -- print =<< evalRandIO (shuffle [1 .. 10])

shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return $ x' : xs'

choose :: (RandomGen g) => NonEmpty a -> Rand g a
choose xs = (xs !!) <$> getRandomR (0, length xs - 1)

newtype Range a = Range { unRange :: (a ,a) }

data Error = FromGreaterThanTo
           | NotAnInteger String
           | NotEven
           | NotOdd
           | InvalidFromEvenToOddRange
           deriving Show

mkRange :: Ord a => a -> a -> Validation [Error] (Range a)
mkRange from to =
  Range <$> validate [FromGreaterThanTo] (uncurry (<)) (from, to)

mkInteger :: Validate f => String -> f [Error] Int
mkInteger s = case readMaybe s of
  Just x  -> _Success # x
  Nothing -> _Failure # [NotAnInteger s]

getInt :: String -> Either [Error] Int
getInt = mkInteger

validateInt :: Validate f => Error -> (Int -> Bool) -> Int -> f [Error] ()
validateInt e isValid x | isValid x = _Success # ()
                        | otherwise = _Failure # [e]

validateOdd, validateEven :: Validate f => Int -> f [Error] ()
validateOdd = validateInt NotOdd odd
validateEven = validateInt NotEven even

parseInt :: (Int -> Either [Error] ()) -> String -> Either [Error] Int
parseInt valiateInt s = do
  i <- mkInteger s

  _Failure # [NotOdd]
--   i <- getInt
--  where
--   getInt :: Either [Error] Int
--   getInt = mkInteger s

-- parseInt :: Validate f => (Int -> f [Error] ()) -> String -> Either [Error] Int
-- parseInt v s = mkInteger s >>= \i -> v i >> return i

-- fromEither $ do
--   i <- toEither $ mkInteger x
--   toEither $ i <$ v i

-- parseEvenInt = parseInt (validateInt NotEven even)
-- parseOddInt = parseInt (validateInt NotOdd odd)
