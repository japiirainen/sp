module Spotify.CLI where

import Options.Applicative

data Command
  = Authorize
  | Play
  | Pause
  deriving stock (Show)

data Options = Options
  { debug :: Bool
  , userCommand :: Command
  }
  deriving stock (Show)

parser :: Parser Options
parser =
  Options
    <$> switch (long "debug" <> short 'd' <> help "Weather to print debug information or not")
    <*> subparser
      ( command "authorize" (info (pure Authorize) (progDesc "Run spotify authorization flow"))
          <> command "play" (info (pure Play) (progDesc "Play current song"))
          <> command "pause" (info (pure Pause) (progDesc "Pause current song"))
      )

parserInfo :: ParserInfo Options
parserInfo =
  info
    (helper <*> parser)
    (progDesc "Command-line utility for the `fp` programming language")

getOpts :: IO Options
getOpts = execParser parserInfo
