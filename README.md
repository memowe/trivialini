# trivialini

**Ultra light weight ini file parser, written in Haskell**

[![Build Status](https://travis-ci.com/memowe/trivialini.svg?branch=main)](https://travis-ci.com/memowe/trivialini)

## Overview

Consider a simple ini-like file `config.ini` like this:

```
foo = bar

[something else]
answer = 42
name = Boaty McBoatface
```

There are two *sections* (inbetween `[` and `]`) defined, `something else` and the anonymous section before. These sections contain a dictionary of Strings each, the keys being something without whitespace, followed by `=` and anything else until end of the line.

With **trivialini**, this data structure can be accessed like this:

```
GHCi, version 8.6.5
Prelude> :m Trivialini
Prelude Trivialini> configFile "config.ini" "something else" "name"
"Boaty McBoatface"
Prelude Trivialini> configFile "config.ini" "default" "foo"
"bar"
```

`configFile`'s first argument is interpreted as a filename, followed by the section and key of the requested value.

There's also a pure function `config` that works on a source String instead of a file name and a function `completeConfig` that returns the complete `Config` data structure for a given file name.

## Author and License

Copyright (c) 2021 Mirko Westermeier

Released under the MIT license. See [LICENSE](LICENSE) for details.
