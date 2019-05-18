-- https://two-wrongs.com/a-gentle-introduction-to-monad-transformers
-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html
{-# LANGUAGE OverloadedStrings #-}

module GentleIntro where

import           Data.Text
import qualified Data.Text.IO                  as T
import           Data.Map                      as Map

data LoginError = InvalidEmail | NoSuchUser | WrongPassword deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email = case splitOn "@" email of
  [_, domain] -> Right domain
  _           -> Left InvalidEmail

newtype ExceptIO e a = ExceptIO { runExceptIO :: IO (Either e a) }

instance Functor (ExceptIO e) where
  fmap f = ExceptIO . fmap (fmap f) . runExceptIO

-- http://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Except.html#ExceptT
instance Applicative (ExceptIO e) where
  pure a = ExceptIO $ return (Right a)
  ExceptIO f <*> ExceptIO v = ExceptIO $ do
    mf <- f
    case mf of
      Left  e -> return (Left e)
      Right k -> do
        mv <- v
        case mv of
          Left  e -> return (Left e)
          Right x -> return (Right (k x))

instance Monad (ExceptIO e) where
  return = pure
  m >>= k = ExceptIO $ do
    a <- runExceptIO m
    case a of
      Left  e -> return (Left e)
      Right x -> runExceptIO (k x)

liftEither :: Either e a -> ExceptIO e a
liftEither = ExceptIO . return

liftIO :: IO a -> ExceptIO e a
liftIO = ExceptIO . fmap Right

throwE :: e -> ExceptIO e a
throwE = liftEither . Left

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler = ExceptIO $ do
  result <- runExceptIO throwing
  case result of
    Left failure -> runExceptIO (handler failure)
    success      -> return success

getToken :: ExceptIO LoginError Text
getToken = do
  liftIO $ T.putStrLn "Enter email address"
  input <- liftIO T.getLine
  liftEither (getDomain input)

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

userLogin :: ExceptIO LoginError Text
userLogin = do

  token    <- getToken
  userpw   <- maybe (throwE NoSuchUser) return (Map.lookup token users)
  password <- liftIO (T.putStrLn "Enter your password:" >> T.getLine)

  if userpw == password then return token else throwE WrongPassword

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO (T.putStrLn "Wrong passord, one more chance")
  userLogin
wrongPasswordHandler err = throwE err

printResult :: Either LoginError Text -> IO ()
printResult res = T.putStrLn $ case res of
  Right token         -> "Logged in with token: " <> token
  Left  InvalidEmail  -> "Invalid Email"
  Left  NoSuchUser    -> "No such user"
  Left  WrongPassword -> "Wrong password"

printError :: LoginError -> ExceptIO LoginError a
printError err = do
  liftIO . T.putStrLn $ case err of
    WrongPassword -> "Wrong password, No more chances"
    NoSuchUser    -> "No user with that email exists"
    InvalidEmail  -> "Invalid email address entered"
  throwE err

loginDialogue :: ExceptIO LoginError ()
loginDialogue = do
  let retry = userLogin `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  liftIO $ T.putStrLn ("Logged in with token: " <> token)
