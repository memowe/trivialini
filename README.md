# trivialini

**Ultra light weight ini file parser, written in Haskell**

[![Test & API Docs pipeline](https://github.com/memowe/trivialini/actions/workflows/test-apidocs.yml/badge.svg)](https://github.com/memowe/trivialini/actions/workflows/test-apidocs.yml)
[![API docs](https://img.shields.io/badge/Haddock-Documentation-8a80a8?style=flat&logo=haskell&logoColor=lightgray)](https://mirko.westermeier.de/trivialini/Trivialini.html)

## Overview

Consider a simple ini-like file `config.ini` like this:

```
[something]
foo = bar

[something else]
answer = 42
name = Boaty McBoatface
```

There are two *sections* (inbetween `[` and `]`) defined, `something` and `something else`. These sections contain a dictionary of Strings each, the keys being some string followed by `=`, and anything else until end of the line as values. The leading and trailing spaces in section headers, keys and values are trimmed.

**trivialini** simply parses a map of maps of strings from this data:

```bash
$ cat config.ini
[foo]
bar = 42
```

```
λ> ini <- readIniFileStrings "config.ini"
λ> ini ! "foo" ! "bar"
"42"
```

## Contributors

[![Contributor Covenant 2.0](https://img.shields.io/badge/Code%20of%20Conduct-Contributor%20Covenant%202.0-8f761b.svg?style=flat&logo=adguard&logoColor=lightgray)](CODE_OF_CONDUCT.md)

- Alexander Pankoff ([@ccntrq](https://github.com/ccntrq))
- Mirko Westermeier ([@memowe](https://github.com/memowe))

## Author and License

Copyright (c) 2021-2024 Mirko Westermeier

Released under the MIT license. See [LICENSE](LICENSE) for details.
