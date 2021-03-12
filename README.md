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
Prelude Trivialini> iniFileValue "config.ini" "something else" "name"
"Boaty McBoatface"
Prelude Trivialini> iniFileValue "config.ini" "default" "foo"
"bar"
```

`iniFileValue`'s first argument is interpreted as a filename, followed by the section and key of the requested value.

### API overview

**Parsing `Ini` data**

- `readIni` parses Ini data from a given ini string
- `readIniFile` parses Ini data from a file given by file name

**`Ini`data accessors**

- `iniSectionMap` returns the key-value map of an `Ini` for a given section name
- `iniValue` returns a value of an `Ini` for a given section name and key

**Combined**

- `iniFileValue` returns a value for a given section name and key, loaded from a file given by file name

## Author and License

Copyright (c) 2021 Mirko Westermeier

Released under the MIT license. See [LICENSE](LICENSE) for details.
