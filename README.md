# CCG Parser


Clojure implementation of a CCG parser for use on an embodied robot. It has a custom lexicon called hockey-lexicon.

This parser has a situatedness component wherein if an object is "detected" (present in objects-detected list) then the lexical entry will be chosen as a noun. e.g. if a "skate" is detected then (Atom. 'N) is the chosen lexical entry for "skate". This leads to context-dependent parses.

## Usage
```sh
$ java -jar nlu-parser-0.1.0-SNAPSHOT-standalone.jar
```

## References
Mark McConville's M.S. thesis 
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.103.4727&rep=rep1&type=pdf
