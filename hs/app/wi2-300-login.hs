{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE Strict             #-}
{-# LANGUAGE StrictData         #-}
module Main (main) where

import           Control.Applicative
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Options.Applicative
import           RIO
import qualified RIO.ByteString               as B
import qualified RIO.ByteString.Lazy          as BL
import           RIO.Process
import           System.IO.Error
import           Text.HTML.Scalpel.Core
import           Text.Printf
import           Text.Regex.Posix

userAgent :: B.ByteString
userAgent = "wi2-300-login"

envchainGroup :: String
envchainGroup = "wi2-300"

envchainUsername :: String
envchainUsername = "WI2_300_USERNAME"

envchainPassword :: String
envchainPassword = "WI2_300_PASSWORD"

newtype Opts = Opts { responseTimeout :: ResponseTimeout }

parseOpts :: IO Opts
parseOpts = execParser $ info opts (header "Logins to https://service.wi2.ne.jp")
  where
    opts = Opts
      <$> (timeoutFromSeconds <$> (optional . option auto $ short 't'
                                                         <> long "timeout"
                                                         <> metavar "TIMEOUT"
                                                         <> help "Timeout (by second)"))
      <**> helper
    timeoutFromSeconds = \case
      Nothing -> responseTimeoutNone
      Just n  -> responseTimeoutMicro (1000000 * n)

main :: IO ()
main = do
  Opts {..} <- parseOpts
  httpManager <- setupManager responseTimeout
  processContext <- mkDefaultProcessContext
  logOpts <- logOptionsHandle stderr True
  withLogFunc logOpts $ \logFunc -> runRIO Env {..} run
  where
    setupManager managerResponseTimeout = newManager tlsManagerSettings
      { managerResponseTimeout
      , managerModifyRequest = \r ->
          return r { requestHeaders = addUserAgent (requestHeaders r)
                   , redirectCount = 0
                   }
      }
    addUserAgent hs = ("User-Agent", userAgent) : filter (\(k, _) -> k /= "User-Agent") hs

data Env = Env
  { logFunc        :: LogFunc
  , processContext :: ProcessContext
  , httpManager    :: Manager
  }

instance HasLogFunc Env where
  logFuncL = lens logFunc (\e f -> e { logFunc = f })

instance HasProcessContext Env where
  processContextL = lens processContext (\e c -> e { processContext = c })

run :: RIO Env ()
run = do
  username <- envchain envchainUsername
  password <- envchain envchainPassword
  confirmNoRobotsTxt
  token <- getCSRFToken
  login username password token
  where
    confirmNoRobotsTxt =
      void $ send "https://service.wi2.ne.jp/robots.txt"
                  (\req -> req { checkResponse = assertStatusCode [404] })
    getCSRFToken = do
      res <- send "https://service.wi2.ne.jp/wi2net/Login/2/" $ \req ->
        req { checkResponse = assertStatusCode [200, 302] }
      let Response { responseStatus = Status code _, responseBody } = res
      when (code == 302) $ throwIO (userError "You have already logged in")
      case scrapeStringLike responseBody $ attr "value" ("input" @: ["name" @= "postKey"]) of
        Nothing    -> throwIO (userError "Could not extract a CSRF token")
        Just token -> return (BL.toStrict token)
    login username password token = do
      res <- send "https://service.wi2.ne.jp/wi2net/Login/1/?Wi2=1" $ \req ->
        urlEncodedBody [("id", username), ("pass", password), ("postKey", token)]
                       req { checkResponse = assertStatusCode [200, 302] }
      case lookup hLocation . responseHeaders $ res of
        Just l | isCorrectLocation l -> B.putStr "Successfully logged in.\n"
        _      -> throwIO $ userError "Failed to login"
    isCorrectLocation l =
      l =~ ("\\`/wi2net/Top/2/?(\\?SSID=[0-9a-f]+)?/?\\'" :: B.ByteString) :: Bool

envchain :: String -> RIO Env B.ByteString
envchain envvar =
  proc "envchain" [envchainGroup, "sh", "-c", "printf %s $" <> envvar] $ \conf ->
    BL.toStrict <$> readProcessStdout_ (closeAllPipes conf)
  where
    closeAllPipes = setStdin closed . setStdout closed . setStderr closed

send :: String -> (Request -> Request) -> RIO Env (Response BL.ByteString)
send url modifyReq = do
  Env { httpManager } <- ask
  req <- modifyReq <$> parseRequest url
  logDebug $ displayBytesUtf8 $ method req <> ": " <> fromString url
  res <- liftIO $ httpLbs req httpManager
  let Response { responseStatus = Status code mes } = res
  logDebug $ displayShow code <> displayBytesUtf8 (" " <> mes)
  return res

assertStatusCode :: (MonadIO m) => [Int] -> a -> Response b -> m ()
assertStatusCode expected _ Response { responseStatus = Status actual _ } =
  unless (actual `elem` expected) $
    throwIO (userError (printf "Expected %s, got %d" (show expected) actual))
