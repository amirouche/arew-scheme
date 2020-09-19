SOURCES = 					\
	README.md				\

PREFIX=$(PWD)

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

okvslite:
	which tclsh || echo "Install tclsh from tcl package"
	mkdir -p local/src
	mkdir -p local/lib
	rm -rf local/src/sqlite
	cd $(PREFIX)/local/src && git clone --depth=1 https://github.com/sqlite/sqlite/
	cd $(PREFIX)/local/src/sqlite && cp ../MakefileLSM .
	cd $(PREFIX)/local/src/sqlite/ && make -f MakefileLSM lsm.so
	cd $(PREFIX)/local/src/sqlite/ && cp lsm.so ../../lib/

termbox:
	mkdir -p local/src
	rm -rf local/src/termbox-truecolor
	cd local/src && git clone https://github.com/amirouche/termbox-truecolor
	cd local/src/termbox-truecolor/ && ./waf configure
	cd local/src/termbox-truecolor/ && ./waf
	mkdir local/lib
	cp local/src/termbox-truecolor/build/src/libtermbox.so local/lib/

chez:
	mkdir -p $(PREFIX)/local/src
	sudo apt install git uuid-dev
	cd $(PREFIX)/local/src && git clone --recursive --depth=1 https://github.com/cisco/ChezScheme/
	cd $(PREFIX)/local/src/ChezScheme/ && ./configure --disable-x11 --disable-curses --threads
	cd $(PREFIX)/local/src/ChezScheme/ && make
	cd $(PREFIX)/local/src/ChezScheme/ && sudo make install

init: chez

doc:
	cat $(SOURCES) > arew-scheme.md
	pandoc arew-scheme.md -o arew-scheme.html
	pandoc arew-scheme.html -o arew-scheme.pdf

repl: ## repl for the win
	@./run

profile-clean:
	rm -rf profile
	mkdir -p profile

check: profile-clean ## run tests using the library test runner
	./venv scheme --program src/arew.scm check src/check-check.scm
	./venv scheme --program src/arew.scm check src/

todo: ## Things that should be done
	@grep -nR --color=always  --before-context=2  --after-context=2 TODO src/

xxx: ## Things that require attention
	@grep -nR --color=always --before-context=2  --after-context=2 XXX src/

clean: ## Remove useless files...
	rm arew-scheme.*
