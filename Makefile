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
	mkdir -p local/lib
	cp local/src/termbox-truecolor/build/src/libtermbox.so local/lib/

blake3:
	mkdir -p local/src
	rm -rf local/src/BLAKE3
	cd local/src && git clone https://github.com/BLAKE3-team/BLAKE3/
	cd local/src/BLAKE3/c && gcc -shared -O3 -o libblake3.so blake3.c blake3_dispatch.c blake3_portable.c     blake3_sse2_x86-64_unix.S blake3_sse41_x86-64_unix.S blake3_avx2_x86-64_unix.S     blake3_avx512_x86-64_unix.S
	mkdir -p local/lib
	cp local/src/BLAKE3/c/libblake3.so local/lib/

chez:
	mkdir -p $(PREFIX)/local/src
	sudo apt install git uuid-dev
	cd $(PREFIX)/local/src && git clone --recursive --depth=1 https://github.com/cisco/ChezScheme/
	cd $(PREFIX)/local/src/ChezScheme/ && ./configure --disable-x11 --disable-curses --threads
	cd $(PREFIX)/local/src/ChezScheme/ && make
	cd $(PREFIX)/local/src/ChezScheme/ && sudo make install

fdb:
	wget https://www.foundationdb.org/downloads/6.2.27/ubuntu/installers/foundationdb-clients_6.2.27-1_amd64.deb
	sudo dpkg -i foundationdb-clients_6.2.27-1_amd64.deb
	wget https://www.foundationdb.org/downloads/6.2.27/ubuntu/installers/foundationdb-server_6.2.27-1_amd64.deb
	sudo dpkg -i foundationdb-server_6.2.27-1_amd64.deb

init: chez fdb

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
