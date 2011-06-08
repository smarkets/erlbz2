erlbz2
======

erlbz2 is a linked-in driver which supports the low-level libbzip2 compression library.

## Requirements

* Erlang R12B1 or greater
* libbzip2 (tested with 1.0.5)

## Source

erlbz2's canonical Git repo is available on Github at:

    http://github.com/smarkets/erlbz2

## Quickstart

    # Clone the repo
    git clone https://github.com/smarkets/erlbz2.git

    # Fetch deps and build it
    # *NOTE: proper may fail to build at first, so run make until it
    # succeeds. I believe this is due to an oddity in rebar where
    # warnings are treated as errors.
    $ make

    # Run tests to verify
    $ make check

### Compression

    1> erlbz2:compress(<<"foo">>).
    <<66,90,104,57,49,65,89,38,83,89,73,254,196,165,0,0,0,1,0,
      1,0,160,0,33,0,130,44,93,201,...>>

### Decompression

    2> erlbz2:decompress(v(1)).
    <<"foo">>

Copyright
---------

Copyright (c) 2011 Smarkets Limited. See LICENSE for details.

Based on original code which is Copyright Ericsson AB 2003-2010. All
Rights Reserved.
