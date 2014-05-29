magmod
======


## Overview
A magento module generator

## Installation
- Save the 'magmod' binary to a location in your PATH (i.e. `/usr/local/bin/`)

#### Downloads
- [magmod-osx-1.0.0-x64](https://drive.google.com/uc?id=0B3X9GlR6EmbnaFo5TllrbFdITUk)
- [magmod-linux-1.0.0-x64](https://drive.google.com/uc?id=0B3X9GlR6EmbnLTBocG1sMXhWclk)

#### Build from source
    cabal install --only-dependencies
    cabal build

## Usage
    magmod new local <namespace> <name>
    magmod new community <namespace> <name>
    magmod add helper <name>
    magmod add model <name>
    magmod add block <name>
    magmod add controller frontend <name>
    magmod add controller admin <name>
    magmod add resource <name>
    magmod add observer frontend <event>
    magmod add observer admin <event>
    magmod add observer global <event>
    magmod add layout frontend
    magmod add layout admin
    magmod add template frontend <name>
    magmod add template admin <name>
    magmod add skin frontend <name>
    magmod add skin admin <name>
    magmod add js <name>
    magmod add locale frontend <locale>
    magmod add locale admin <locale>
    magmod add install
    magmod add upgrade
    magmod gen layout frontend
    magmod gen layout admin
    magmod clean
