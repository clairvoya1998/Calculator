calc: Expr.hs Main.hs Parsing.hs REPL.hs
	ghc --make Main.hs -o calc -outputdir build

clean:
	rm -rf build calc
