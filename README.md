spotify

## Table of contents

- [Table of contents](#table-of-contents)
- [A spotify CLI](#a-spotify-cli)
- [Usage of `spotify`](#usage-of-spotify)
  - [Authorization](#authorization)
  - [Basic commands](#basic-commands)
- [Development](#development)
  - [Nix support](#nix-support)
  - [Tips](#tips)

## A spotify CLI

## Usage of `spotify`

### Authorization

### Basic commands

## Development

You can develop `spotify` with `cabal`.

```sh
cabal build all

cabal run spotify
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
- Run the application without installing: `nix run github:japiirainen/spotify` (or `nix run .` from checkout)
