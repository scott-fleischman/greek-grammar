## Modeling the Morphophonology of Ancient Greek
A Human-directed Computational Approach

### Building
[![Build Status](https://travis-ci.org/scott-fleischman/greek-grammar.svg?branch=master)](https://travis-ci.org/scott-fleischman/greek-grammar)

- Download [Stack](https://github.com/commercialhaskell/stack/wiki)
- Run the following commands in a terminal:
```Shell
git clone git://github.com/scott-fleischman/greek-grammar.git
cd greek-grammar
git submodule update --init
stack build
```

### Running
Generate data:
```Shell
stack exec run
```

View data: start a web server in the `html/dev` directory.

### Documents
[SBL 2015 proposal](docs/2015-03 SBL Proposal.md)

[SBL 2015 slides](docs/2015-11 SBL Slides.pdf)
