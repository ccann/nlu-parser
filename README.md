# nlu-parser

This is my implementation of a ccg parser for Situated Natural Language Understanding on Robots course. It has a custom lexicon called hockey-lexicon.

It has a situatedness component wherein if an object is detected (in objects-detected) then the lexical entry will be chosen as a noun. e.g. if a "skate" is detected then (Atom. 'N) is the chosen lexical entry for "skate". This leads to context-dependent parses.

## Usage

java -jar nlu-parser-0.1.0-SNAPSHOT-standalone.jar



