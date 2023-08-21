sp

## Table of contents

- [Table of contents](#table-of-contents)
- [A spotify CLI](#a-spotify-cli)
  - [Installation](#installation)
    - [`nix`](#nix)
- [Usage of `sp`](#usage-of-sp)
  - [Authorization](#authorization)
  - [Basic commands](#basic-commands)
    - [`authorize`](#authorize)
    - [`play`](#play)
    - [`pause`](#pause)
    - [`next`](#next)
    - [`prev`](#prev)
    - [`replay`](#replay)
    - [`seek`](#seek)
    - [`track`](#track)
- [Development](#development)
  - [Nix support](#nix-support)
  - [Tips](#tips)

## A spotify CLI

### Installation

You can install `sp` via the following mechanisms.

#### `nix`

`sp` is a nix `flake`, so you can run it directly from this github repo without installing at all!

```bash
nix run github:japiirainen/sp -- --help
```

#### `cabal`

```bash
cabal install
```

## Usage of `sp`

### Authorization

1. Got to <a href="https://developer.spotify.com/dashboard/">Spotify dashboard</a> and register an app

2. In options, set Redirect URIs to "http://localhost:7777/callback"

3. Install `sp` via `nix` or `cabal`.

4. Run `sp authorize` and follow instructions (paste in client-id and client-secret from spotify application when prompted)

5. Done!

### Basic commands

#### `authorize`

Run authoration flow.

```bash
sp authorize
```

#### `play`

Play current song.

```bash
sp play
```

#### `pause`

Pause current song.

```bash
sp pause
```

#### `next`

Skips to next track in queue.

```bash
sp next
```

#### `prev`

Skips to previous track in queue.

```bash
sp prev
```

#### `replay`

Replay current song from the beginning.

```bash
sp replay
```

#### `seek`

Seeks to the given position in the currently playing track.

```bash
sp seek 60
```

#### `track`

Search for tracks.

```bash
sp track 'Ehtaa tavaraa'
```

#### `album`

Search for tracks.

```bash
sp album 'After Hours'
```

## Development

You can develop `sp` with `cabal`.

```sh
cabal build all

cabal run sp
```

### Nix support

You can alternatively use nix for dev environment and for building the project.

Build:

```sh
nix build .
```

Run:

```sh
nix run .
```

Start Nix shell:

```sh
nix-shell
```

### Tips

- Run `nix flake update` to update all flake inputs.
- Run `just fmt` to run formatters.
- Run `just docs` to start  local hoogle server.
- Run the application without installing: `nix run github:japiirainen/sp` (or `nix run .` from checkout)
