# Revision history for trivialini

## ????

* Breaking changes:
    * Change Ini type alias back to full type
    * Add stringification and parsing as Show and Read instances
    * Merge small modules

## 0.3.1.0 -- 2021-07-27

* Allow whitespace in keys (Alexander Pankoff @ccntrq)
* Make section headers a little bit more strict
* Improve test suite
* Add Haddock API docs generation to GitHub Pages
* Improve parser readability

## 0.3.0.0 -- 2021-07-14

* Breaking changes:
    * Ini = Map of Map of Strings from now on
    * No default section anymore (it was just a regex workaround anyway)
* Switch from regex to parser combinators
* Allow arbitrary whitespace inbetween key-value pairs
* Split into single-purpose modules

## 0.2.0.0 -- 2021-03-12

* Breaking API changes:
    * use "ini" instead of "config" everywhere
    * Export all accessor functions

## 0.1.4.0 -- 2021-03-12

* Minor code cleanups
* Make readConfig (String parsing) public
* Add a super-simple test suite

## 0.1.3.0 -- 2021-03-10

* Add a completeConfig method for the whole data structure

## 0.1.2.0 -- 2021-03-07

* Simplify regex matching
* Use TDFA instead of PCRE to minimize external dependencies

## 0.1.1.0 -- 2021-03-07

* Add internal data type simplification (shows with Show)

## 0.1.0.0 -- 2021-02-22

* First version. Released on an unsuspecting world.
