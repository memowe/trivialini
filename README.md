# trivialini

**Ultra light weight ini file parser, written in Haskell**

[![Build and test](https://github.com/memowe/trivialini/actions/workflows/test.yml/badge.svg)](https://github.com/memowe/trivialini/actions/workflows/test.yml)

## Overview

Consider a simple ini-like file `config.ini` like this:

```
[something]
foo = bar

[something else]
answer = 42
name = Boaty McBoatface
```

There are two *sections* (inbetween `[` and `]`) defined, `something` and `something else`. These sections contain a dictionary of Strings each, the keys being some string followed by `=`, and anything else until end of the line as values.

**trivialini** parses this data structure as an `Ini`, which is simply a map of maps of strings:

```bash
$ cat config.ini
[foo]
bar = 42
```

```
*Trivialini Data.Map> ini <- readIniFile "config.ini"
*Trivialini Data.Map> ini ! "foo" ! "bar"
"42"
```

## Contributors

- Alexander Pankoff (@ccntrq)

## Author and License

Copyright (c) 2021 Mirko Westermeier

Released under the MIT license. See [LICENSE](LICENSE) for details.
