

all: main

run: all
	cd text-editor-main; \
	cabal run src/TTYRender.hs

main: text-editor-main/src/*.hs
	cd text-editor-main; \
	cabal build

clean: cleanmain

cleanmain:
	cd text-editor-main; \
	cabal clean

