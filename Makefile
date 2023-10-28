SOURCE_FILES := src/Main.hs src/GlyphCompletion.hs src/Lex.hs

bin/ai: $(SOURCE_FILES)
	cd src; ghc -no-keep-hi-files -no-keep-o-files -package haskeline -o ../bin/ai Main.hs

clean:
	rm bin/ai
