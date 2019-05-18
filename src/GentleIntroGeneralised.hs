{-# LANGUAGE OverloadedStrings #-}

module GentleIntroGeneralised where

import           Data.Text                                ( Text
                                                          , splitOn
                                                          )
import qualified Data.Text.IO                  as T
import qualified Data.Map                      as Map
import           Data.Map                                 ( Map )
import           Control.Applicative                      ( liftA2 )
import           Control.Monad                            ( void )

main :: IO ()
main = void $ runExceptT loginDialogue

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
  let retry = userLogin `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  lift $ T.putStrLn ("Logged in with token: " <> token)

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
  lift $ T.putStrLn "Wrong password, one more chance"
  userLogin
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
  lift . T.putStrLn $ case err of
    WrongPassword -> "Wrong password, no more chances"
    NoSuchUser    -> "No user with that email exists"
    InvalidEmail  -> "Invalid email address entered"
  throwE err

userLogin :: ExceptT LoginError IO Text
userLogin = do
  token    <- getToken
  userpw   <- maybe (throwE NoSuchUser) return (Map.lookup token users)
  password <- lift (T.putStrLn "Enter your password:" >> T.getLine)
  if userpw == password then return token else throwE WrongPassword

getToken :: ExceptT LoginError IO Text
getToken = do
  lift (T.putStrLn "Enter email address")
  input <- lift T.getLine
  liftEither (getDomain input)

getDomain :: Text -> Either LoginError Text
getDomain email = case splitOn "@" email of
  [_, domain] -> Right domain
  _           -> Left InvalidEmail

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

-- XXX: Unlawful instance. Don't user for anything serious.
instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  f <*> x = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT x)

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  x >>= f = ExceptT $ runExceptT x >>= either (return . Left) (runExceptT . f)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither = ExceptT . return

lift :: Functor m => m a -> ExceptT e m a
lift = ExceptT . fmap Right

throwE :: Monad m => e -> ExceptT e m a
throwE = liftEither . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT c m a) -> ExceptT c m a
catchE throwing handler = ExceptT $ do
  x <- runExceptT throwing
  case x of
    Left  failure -> runExceptT (handler failure)
    Right success -> return (Right success)
