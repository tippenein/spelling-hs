all: file

file:
	stack build

memory:
	stack exec spelling-hs -- +RTS -s
	stack exec spelling-hs -- +RTS -s
html:
	bench 'stack exec spelling-hs -- toHistogram' --output bench/toHistogram.html
	bench 'stack exec spelling-hs -- words' --output bench/words.html
	bench 'stack exec spelling-hs -- proba' --output bench/proba.html
	bench 'stack exec spelling-hs -- all' --output bench/all.html

benchmark:
	bench 'stack exec spelling-hs -- toHistogram'
	bench 'stack exec spelling-hs -- words'
	bench 'stack exec spelling-hs -- proba'
	bench 'stack exec spelling-hs -- all'
