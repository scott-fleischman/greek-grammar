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
#### Generate data
```Shell
stack exec run
```

#### View data
* Start a web server in the `html/dev` directory.
* You may also view it at: https://scott-fleischman.github.io/greek-grammar

### Documents
[LambdaConf 2016 slides](docs/2016-05%20LambdaConf%20Slides.pdf) / [video](https://www.youtube.com/watch?v=DA1-swDeeCE)

[SBL 2015 slides](docs/2015-11%20SBL%20Slides.pdf)

[SBL 2015 proposal](docs/2015-03%20SBL%20Proposal.md)
