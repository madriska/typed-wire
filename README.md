typed-wire
=====

[![Build Status](https://travis-ci.org/agrafix/typed-wire.svg)](https://travis-ci.org/agrafix/typed-wire)
[![Hackage](https://img.shields.io/hackage/v/typed-wire.svg)](http://hackage.haskell.org/package/typed-wire)

## Intro

Hackage: [typed-wire](http://hackage.haskell.org/package/typed-wire)

WIP: Language idependent type-safe communication

## Cli Usage: twirec

```sh
$ twirec --help
Generate bindings using typed-wire for different languages

Usage: twirec [--version] [-i|--include-dir DIR] [-e|--entrypoint MODULE-NAME]
              [--hs-out DIR] [--elm-out DIR]
  Language idependent type-safe communication

Available options:
  -h,--help                Show this help text
  --version                Show version and exit
  -i,--include-dir DIR     Directory to search for modules
  -e,--entrypoint MODULE-NAME
                           Entrypoint for compiler
  --hs-out DIR             Generate Haskell bindings to specified dir
  --elm-out DIR            Generate Elm bindings to specified dir

```

## Install

* Using cabal: `cabal install typed-wire`
* From Source (cabal): `git clone https://github.com/agrafix/typed-wire.git && cd typed-wire && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/typed-wire.git && cd typed-wire && stack build`


## Misc

### Supported GHC Versions

* 7.10.2

### License

Released under the MIT license.
(c) 2015 Alexander Thiemann <mail@athiemann.net>
