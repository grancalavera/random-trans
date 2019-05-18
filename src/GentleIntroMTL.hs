{-# LANGUAGE OverloadedStrings #-}

module GentleIntroMTL where

import           Data.Text                                ( Text
                                                          , splitOn
                                                          )
import qualified Data.Text.IO                  as T
import qualified Data.Map                      as Map
import           Data.Map                                 ( Map )
import           Control.Applicative                      ( liftA2 )
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

loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
  let retry = userLogin `catchError` wrongPasswordHandler
  token <- retry `catchError` printError
  lift $ T.putStrLn ("Logged in with token: " <> token)

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
  lift $ T.putStrLn "Wrong password, one more chance"
  userLogin
wrongPasswordHandler err = throwError err

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
  lift . T.putStrLn $ case err of
    WrongPassword -> "Wrong password, no more chances"
    NoSuchUser    -> "No user with that email exists"
    InvalidEmail  -> "Invalid email address entered"
  throwError err

userLogin :: ExceptT LoginError IO Text
userLogin = do
  token    <- getToken
  userpw   <- maybe (throwError NoSuchUser) return (Map.lookup token users)
  password <- lift (T.putStrLn "Enter your password:" >> T.getLine)
  if userpw == password then return token else throwError WrongPassword

getToken :: ExceptT LoginError IO Text
getToken = do
  lift (T.putStrLn "Enter email address")
  input <- lift T.getLine
  liftEither (getDomain input)

getDomain :: Text -> Either LoginError Text
getDomain email = case splitOn "@" email of
  [_, domain] -> Right domain
  _           -> Left InvalidEmail
