build :
	cabal build

run : 
	@#cabal run hcat -- urmom is fat 4269 "unquoted" \"quoted\" "with  spaces"
	@#cabal run hcat -- ./test/input.txt
	cabal run hcat -- ./hcat.cabal
	@#cabal run hcat -- nofile
	@#cabal run hcat

doc: ## Create docs/scc.html
	@echo "Creating scc documentation in html"
	@mkdir -p "docs"
	@touch "docs/scc.html"
	@sccount --overhead 1.0 --no-gen -n "scc.html" -s "complexity" -f "html" > docs/scc.html

