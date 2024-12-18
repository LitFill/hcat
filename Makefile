build :
	cabal build

run : 
	@#cabal run hcat -- urmom is fat 4269 "unquoted" \"quoted\" "with  spaces"
	@#cabal run hcat -- ./test/input.txt
	@#cabal run hcat -- ./hcat.cabal
	cabal run hcat -- nofile
	@#cabal run hcat
