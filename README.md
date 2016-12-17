Spell Correct
----

A reimplementation of Norvig's spell check in haskell

http://norvig.com/spell-correct.html

processed txt file with:
```
cat big.txt | tr 'A-Z' 'a-z' | sed 's/[^a-z]/ /g' > big-words.txt
```
