module Spotify.CLI where

import Options.Applicative

data Command
  = Authorize
  | Play
  | Pause
  | Next
  | Prev
  | Replay
  | Seek Int

instance Show Command where
  show = \case
    Authorize -> "Authorize"
    Play -> "play"
    Pause -> "pause"
    Next -> "next"
    Prev -> "prev"
    Replay -> "replay"
    Seek s -> "seek " <> show s

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
          <> command "next" (info (pure Next) (progDesc "Skips to next track in the queue."))
          <> command "prev" (info (pure Prev) (progDesc "Skips to previous track in the queue."))
          <> command "replay" (info (pure Replay) (progDesc "Replay current song from the beginning."))
          <> command "seek" (info (Seek <$> argument (auto @Int) (help "Where to seek in seconds.")) (progDesc "Seeks to the given position in the currently playing track."))
      )

parserInfo :: ParserInfo Options
parserInfo =
  info
    (helper <*> parser)
    (progDesc "Command-line utility for the `fp` programming language")

getOpts :: IO Options
getOpts = execParser parserInfo
