module Sp.CLI where

import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative

data Command
  = Authorize
  | Play
  | Pause
  | Next
  | Prev
  | Replay
  | Seek Int
  | SearchTrack Text
  | SearchAlbum Text

instance Show Command where
  show = \case
    Authorize -> "Authorize"
    Play -> "play"
    Pause -> "pause"
    Next -> "next"
    Prev -> "prev"
    Replay -> "replay"
    Seek s -> "seek " <> show s
    SearchTrack q -> "search `" <> Text.unpack q <> "`"
    SearchAlbum q -> "search `" <> Text.unpack q <> "`"

data Options = Options
  { debug :: Bool
  , userCommand :: Command
  }
  deriving stock (Show)

parser :: Parser Options
parser =
  Options
    <$> switch (long "debug" <> short 'd' <> help "Weather to print debug information or not.")
    <*> subparser
      ( command "authorize" (info (pure Authorize) (progDesc "Run `sp` authorization flow."))
          <> command "play" (info (pure Play) (progDesc "Play current song."))
          <> command "pause" (info (pure Pause) (progDesc "Pause current song."))
          <> command "next" (info (pure Next) (progDesc "Skips to next track in the queue."))
          <> command "prev" (info (pure Prev) (progDesc "Skips to previous track in the queue."))
          <> command "replay" (info (pure Replay) (progDesc "Replay current song from the beginning."))
          <> command "seek" (info (Seek <$> argument (auto @Int) (help "Where to seek in seconds.")) (progDesc "Seeks to the given position."))
          <> command "track" (info (SearchTrack <$> strArgument (help "Search query.")) (progDesc "Search for tracks."))
          <> command "album" (info (SearchAlbum <$> strArgument (help "Search album.")) (progDesc "Search for an album."))
      )

parserInfo :: ParserInfo Options
parserInfo =
  info
    (helper <*> parser)
    (progDesc "`sp` is a CLI for simple interactions with spotify.")

getOpts :: IO Options
getOpts = execParser parserInfo
