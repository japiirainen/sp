sp

## Table of contents

- [Table of contents](#table-of-contents)
- [A spotify CLI](#a-spotify-cli)
- [Usage of `sp`](#usage-of-sp)
  - [Authorization](#authorization)
  - [Basic commands](#basic-commands)
- [Development](#development)
  - [Nix support](#nix-support)
  - [Tips](#tips)

## A spotify CLI

## Usage of `sp`

### Authorization

### Basic commands

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
