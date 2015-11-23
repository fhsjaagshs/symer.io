module Blog.CommandLine
(
  Cmd(..),
  getCommand,
  getCommandArgs,
)
where

import Blog.System.Terminal

import Control.Applicative
import Options.Applicative
import System.Environment (getArgs)

data Cmd
  = StartCommand {
    startCmdDaemonize :: Bool,
    startCmdPort :: Int,
    startCmdHTTPSSLCert :: FilePath,
    startCmdHTTPSSLKey :: FilePath,
    startCmdPGSSLRootCrt :: FilePath,
    startCmdPGSSLCert :: FilePath,
    startCmdPGSSLKey :: FilePath,
    startCmdOutputPath :: Maybe FilePath,
    startCmdErrorPath :: Maybe FilePath
  }
  | StopCommand
  | StatusCommand
  | PasswordCommand {
    passwordCmdPassword :: String
  }

getCommand :: IO Cmd
getCommand = getArgs >>= getCommandArgs

getCommandArgs :: [String] -> IO Cmd
getCommandArgs args = do
  w <- maybe 80 snd <$> getTermSize
  handleParseResult $ execParserPure (pprefs w) parser args
  where
    pprefs = ParserPrefs "" False False True
    parser = info (helper <*> parseCommand) (fullDesc <> progDesc "Nathaniel Symer's blog." <> header "blog.symer.io")
    
parseCommand :: Parser Cmd
parseCommand = sp <|> parseStart
  where
    sp = subparser $ (mkcmd "start" "Start the blog" parseStart) <>
                     (mkcmd "stop" "Stop the blog" parseStop) <>
                     (mkcmd "status" "Determine if the blog is running" parseStatus) <>
                     (mkcmd "password" "Make a BCrypt hash from a password" parseHash)
    parseStart = StartCommand
      <$> (flag False True $ short 'd')
      <*> (option auto $ opt "port" 'p' "PORT" (Just 3000) "port to run blog on.")
      <*> (strOption $ opt "https-crt" 'c' "FILEPATH" (Just "server.crt") ".crt file used for SSL")
      <*> (strOption $ opt "https-key" 'k' "FILEPATH" (Just "server.key") ".key file used for SSL")
      <*> (strOption $ opt "pg-root-crt" 'x' "FILEPATH" (Just "root.crt") "root CA certificate in the SSL chain of trust used by Postgres")
      <*> (strOption $ opt "pg-crt" 'y' "FILEPATH" (Just "postgres.crt") "client certificate used to connect to a Postgres database")
      <*> (strOption $ opt "pg-key" 'z' "FILEPATH" (Just "postgres.key") "client private key used to connect to a Postgres database")
      <*> (optional $ strOption $ opt "stdout" 'o' "FILEPATH" Nothing "which file to redirect STDOUT to")
      <*> (optional $ strOption $ opt "stderr" 'e' "FILEPATH" Nothing "which file to redirect STDERR to")
    parseStop     = pure $ StopCommand
    parseStatus   = pure $ StatusCommand
    parseHash     = PasswordCommand <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc