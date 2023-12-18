GHC := ghc -dynamic -no-keep-hi-files -no-keep-o-files -package haskeline -package utf8-string -package pcre-light -package mtl
SOURCE_FILES := src/*

bin/ai: $(SOURCE_FILES)
	cd src; $(GHC) -o ../bin/ai Main.hs

clean:
	rm bin/ai
