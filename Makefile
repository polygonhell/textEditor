

all: main

run: all
	cd text-editor-main; \
	cabal exec cabal run src/TTYRender.hs		# Yes really, without the cabal exec setting the GHC_PACKAGE_PATH Hint woun't find the modules


main: text-editor-main/src/*.hs
	cd text-editor-main; \
	cabal build; \

clean: cleanmain


cleanmain:
	cd text-editor-main; \
	cabal clean

