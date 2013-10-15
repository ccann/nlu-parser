# nlu-parser


This is my clojure implementation of a ccg parser for use on an embodied robot. It has a custom lexicon called hockey-lexicon.

It has a situatedness component wherein if an object is detected (in objects-detected) then the lexical entry will be chosen as a noun. e.g. if a "skate" is detected then (Atom. 'N) is the chosen lexical entry for "skate". This leads to context-dependent parses.

## Usage

java -jar nlu-parser-0.1.0-SNAPSHOT-standalone.jar


## References
Mark McConville's M.S. thesis 
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.103.4727&rep=rep1&type=pdf
A clojure implementation of a combinatory categorical grammar as decribed by 

