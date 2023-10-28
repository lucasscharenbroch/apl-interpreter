GHC := ghc -no-keep-hi-files -no-keep-o-files -package haskeline
SOURCE_FILES := src/*

bin/ai: $(SOURCE_FILES)
	cd src; $(GHC) -o ../bin/ai Main.hs

clean:
	rm bin/ai
