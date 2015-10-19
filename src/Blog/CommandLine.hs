module Blog.CommandLine
(
  Cmd(..),
  getCommand,
  getCommandArgs,
)
where

import Blog.Terminal

import Control.Applicative
import Options.Applicative
import System.Environment (getArgs)

data Cmd
  = StartCommand {
    startCmdDaemonize :: Bool,
    startCmdPort :: Int,
    startCmdHTTPSSLCert :: FilePath,
    startCmdHTTPSSLKey :: FilePath,
    startCmdDatabasePassword :: String,
    startCmdPGSSLCrt :: FilePath,
    startCmdPGSSLKey :: FilePath,
    startCmdPGSSLRootCrt :: FilePath,
    startCmdOutputPath :: Maybe FilePath,
    startCmdErrorPath :: Maybe FilePath
  }
  | StopCommand
  | StatusCommand

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
                     (mkcmd "status" "Determine if the blog is running" parseStatus)
    parseStart = StartCommand
      <$> (flag False True $ short 'd')
      <*> (option auto $ opt "port" 'p' "PORT" (Just 3000) "port to run blog on.")
      <*> (strOption $ opt "https-crt" 'c' "FILEPATH" (Just "server.crt") ".crt file used for SSL")
      <*> (strOption $ opt "https-key" 'k' "FILEPATH" (Just "server.key") ".key file used for SSL")
      <*> (strOption $ opt "pg-passwd" 'l' "PASSWORD" (Just "") "password used to connect to Postgres.")
      <*> (strOption $ opt "pg-crt" 'x' "FILEPATH" (Just "server.crt") "certificate used for SSL-secured Postgres connections, relative to PG data directory.")
      <*> (strOption $ opt "pg-key" 'y' "FILEPATH" (Just "server.key") "private key used for SSL-secured Postgres connections, relative to PG data directory.")
      <*> (strOption $ opt "pg-ca-crt" 'z' "FILEPATH" (Just "root.crt") "root CA certificate in pg-crt's chain of trust")
      <*> (optional $ strOption $ opt "stdout" 'o' "FILEPATH" Nothing "which file to redirect STDOUT to")
      <*> (optional $ strOption $ opt "stderr" 'e' "FILEPATH" Nothing "which file to redirect STDERR to")
    parseStop     = pure $ StopCommand
    parseStatus   = pure $ StatusCommand
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc