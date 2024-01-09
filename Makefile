PACKAGES := -package haskeline -package utf8-string -package pcre-light -package mtl -package random
GHC := ghc -dynamic -no-keep-hi-files -no-keep-o-files $(PACKAGES) -O1
GHCi := ghci -dynamic -fbreak-on-error $(PACKAGES)
SOURCE_FILES := src/*

bin/ai: $(SOURCE_FILES)
	cd src; $(GHC) -o ../bin/ai Main.hs

clean:
	rm bin/ai

debug:
	cd src; $(GHCi) Main.hs
