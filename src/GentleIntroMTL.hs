{-# LANGUAGE OverloadedStrings #-}

module GentleIntroMTL where

import           Data.Text                                ( Text
                                                          , splitOn
                                                          )
import qualified Data.Text.IO                  as T
import qualified Data.Map                      as Map
import           Data.Map                                 ( Map )
import           Control.Applicative                      ( liftA2 )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Except                     ( ExceptT
                                                          , catchError
                                                          , throwError
                                                          , liftEither
                                                          , runExceptT
                                                          )

main :: IO ()
main = do
  runExceptT loginDialogue
  return ()

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

loginDialogue :: MonadIO m => ExceptT LoginError m ()
loginDialogue = do
  let retry = userLogin `catchError` wrongPasswordHandler
  token <- retry `catchError` printError
  liftIO $ T.putStrLn ("Logged in with token: " <> token)

wrongPasswordHandler :: MonadIO m => LoginError -> ExceptT LoginError m Text
wrongPasswordHandler WrongPassword = do
  liftIO $ T.putStrLn "Wrong password, one more chance"
  userLogin
wrongPasswordHandler err = throwError err

userLogin :: MonadIO m => ExceptT LoginError m Text
userLogin = do
  token    <- getToken
  userpw   <- maybe (throwError NoSuchUser) return (Map.lookup token users)
  password <- liftIO (T.putStrLn "Enter your password:" >> T.getLine)
  if userpw == password then return token else throwError WrongPassword

printError :: MonadIO m => LoginError -> ExceptT LoginError m a
printError err = do
  liftIO . T.putStrLn $ case err of
    WrongPassword -> "Wrong password, no more chances"
    NoSuchUser    -> "No user with that email exists"
    InvalidEmail  -> "Invalid email address entered"
  throwError err

getToken :: MonadIO m => ExceptT LoginError m Text
getToken = do
  liftIO (T.putStrLn "Enter email address")
  input <- liftIO T.getLine
  liftEither (getDomain input)

getDomain :: Text -> Either LoginError Text
getDomain email = case splitOn "@" email of
  [_, domain] -> Right domain
  _           -> Left InvalidEmail
