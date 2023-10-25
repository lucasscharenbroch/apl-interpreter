bin/ai: src/Main.hs
	ghc -no-keep-hi-files -no-keep-o-files -o bin/ai src/Main.hs

clean:
	rm bin/ai
