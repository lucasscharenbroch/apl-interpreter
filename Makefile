GHC := ghc -dynamic -no-keep-hi-files -no-keep-o-files -package haskeline -package utf8-string-1.0.2 -package pcre-light-0.4.1.2
SOURCE_FILES := src/*

bin/ai: $(SOURCE_FILES)
	cd src; $(GHC) -o ../bin/ai Main.hs

clean:
	rm bin/ai
