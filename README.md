# A Microformats V2 Parser

Disclaimer: This is still a work in progress, some required features are not yet implemented (See the Known Issues section below).

This is a first-pass at a [microformats v2] parser written in Erlang. It is a component of a CouchDB webmention plugin I am working on to making my CouchDB based website indieweb compatible and self-contained (I'm currently using a [Python-based webmention server][indie-flask]).

## Known issues

The following features of the [microformats-v2 parsing algorithm][mf2parsing] are not yet implemented:

- Backcompat
- Embedded content
- No rels/rel-urls parsing
- Possibly some of the implied property rules

## Compile and go

~~~~
$ rebar3 compile
...
# fetch the tests
$ git clone https://github.com/microformats/tests.git tests/mf2
$ ./tests/00-mf2-parser.t
~~~~

[microformats v2]: http://microformats.org
[mf2parsing]: http://microformats.org/wiki/microformats2-parsing
[indie-flask]: https://github.com/hazybluedot/indie_flask
