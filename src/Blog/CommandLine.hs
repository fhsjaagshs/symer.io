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
    startCmdDatabasePassword :: String,
    startCmdSSLCertfile :: FilePath,
    startCmdSSLKeyfile :: FilePath,
    startCmdOutputPath :: Maybe FilePath,
    startCmdErrorPath :: Maybe FilePath
  }
  | RedirectCommand
  | StopCommand
  | StatusCommand

getCommand :: IO Cmd
getCommand = getArgs >>= getCommandArgs

getCommandArgs :: [String] -> IO Cmd
getCommandArgs [] = getCommandArgs ["start"]
getCommandArgs args = do
  mts <- getTermSize
  handleParseResult $ execParserPure (pprefs $ width mts) parser args
  where
    pprefs = ParserPrefs "" False False True
    width (Just (w, _)) = w
    width _ = 80
    parser = info (helper <*> parseCommand) (fullDesc <> progDesc "Nathaniel Symer's blog." <> header "blog.symer.io")
    
parseCommand :: Parser Cmd
parseCommand = sp <|> parseStart
  where
    sp = subparser ((mkcmd "start" "Start the blog" parseStart) <>
                    (mkcmd "stop" "Stop the blog" parseStop) <>
                    (mkcmd "status" "Determine if the blog is running" parseStatus) <>
                    (mkcmd "redirect-http" "Redirect HTTP to HTTPs" parseRedirect))
    parseStart = StartCommand
      <$> (flag False True $ short 'd')
      <*> (option auto $ opt "port" 'p' "PORT" (Just 3000) "The port to run blog on.")
      <*> (strOption $ opt "dbpasswd" 'l' "PASSWORD" (Just "") "The password used to connect to Postgres.")
      <*> (strOption $ opt "keyfile" 'k' "FILEPATH" (Just "server.key") "The SSL .key file used for SSL.")
      <*> (strOption $ opt "crtfile" 'c' "FILEPATH" (Just "server.crt") "The SSL .crt file used for SSL.")
      <*> (optional $ strOption $ opt "stdout" 'o' "FILEPATH" Nothing "Redirect STDOUT to a file.")
      <*> (optional $ strOption $ opt "stderr" 'e' "FILEPATH" Nothing "Redirect STDERR to a file.")
    parseStop     = pure $ StopCommand
    parseStatus   = pure $ StatusCommand
    parseRedirect = pure $ RedirectCommand
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc