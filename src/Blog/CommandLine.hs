module Blog.CommandLine
(
  Cmd(..),
  getCommand,
  getCommandArgs,
)
where

import qualified System.Console.Terminal.Size as TerminalSize -- for optparse-applicative print width

import Control.Applicative
import Options.Applicative
import System.Environment

data Cmd
  = StartCommand {
    startCmdDaemonize :: Bool,
    startCmdPort :: Int,
    startCmdDatabasePassword :: String,
    startCmdSSLCertfile :: FilePath,
    startCmdSSLKeyfile :: FilePath,
    startCmdOutputPath :: Maybe FilePath,
    startCmdErrorPath :: Maybe FilePath
  }
  | StopCommand
  | StatusCommand

-- options

mkcmd :: String -> String -> Parser a ->  Mod CommandFields a
mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc

parser :: ParserInfo Cmd
parser = info (helper <*> parseCommand) (fullDesc <> progDesc "Nathaniel Symer's blog."
                                                  <> (header $ "blog.symer.io")) 

parseCommand :: Parser Cmd
parseCommand = sp <|> parseStart
  where
    sp = subparser ((mkcmd "start" "Start the blog" parseStart) <>
                    (mkcmd "stop" "Stop the blog" parseStop) <>
                    (mkcmd "status" "Determine if the blog is running" parseStatus)
                    )
    parseStart :: Parser Cmd
    parseStart = StartCommand
      <$> (flag False True (short 'd'))
      <*> (option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "The port to run blog on."))
      <*> (strOption (long "dbpasswd" <> short 'l' <> metavar "PASSWORD" <> value "" <> help "The password used to connect to Postgres."))
      <*> (strOption (long "crtfile" <> short 'c' <> metavar "FILEPATH" <> value "server.crt" <> help "The SSL .crt file used for SSL."))
      <*> (strOption (long "keyfile" <> short 'k' <> metavar "FILEPATH" <> value "server.key" <> help "The SSL .key file used for SSL."))
      <*> (optional $ strOption (long "stdout" <> short 'o' <> metavar "FILEPATH" <> help "Where to redirect STDOUT to. Has no effect unless daemonized."))
      <*> (optional $ strOption (long "stderr" <> short 'e' <> metavar "FILEPATH" <> help "Where to redirect STDERR to. Has no effect unless daemonized."))
    parseStop :: Parser Cmd
    parseStop = pure $ StopCommand
    parseStatus :: Parser Cmd
    parseStatus = pure $ StatusCommand

-- optparse-applicative Helpers

termWidth :: IO Int
termWidth = termWidth' <$> TerminalSize.size
  where
    termWidth' (Just (TerminalSize.Window _ w)) = w :: Int
    termWidth' _ = 10

parserPrefs :: Int -> ParserPrefs
parserPrefs width = ParserPrefs "" False False True width

getCommand :: IO Cmd
getCommand = getArgs >>= getCommandArgs

getCommandArgs :: [String] -> IO Cmd
getCommandArgs [] = getCommandArgs ["serve", "."]
getCommandArgs args = do
  ps <- parserPrefs <$> termWidth
  handleParseResult $ execParserPure ps parser args