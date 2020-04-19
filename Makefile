SOURCES = 					\
	README.md				\
	src/arew/scheme/base.md			\
	src/arew/scheme/bytevector.md		\
	src/arew/scheme/case-lambda.md		\
	src/arew/scheme/char.md			\
	src/arew/scheme/charset.md		\
	src/arew/scheme/complex.md		\
	src/arew/scheme/cxr.md			\
	src/arew/scheme/eval.md			\
	src/arew/scheme/file.md			\
	src/arew/scheme/inexact.md		\
	src/arew/scheme/lazy.md			\
	src/arew/scheme/load.md			\
	src/arew/scheme/process-context.md	\
	src/arew/scheme/r5rs.md			\
	src/arew/scheme/read.md			\
	src/arew/scheme/repl.md			\
	src/arew/scheme/time.md			\
	src/arew/scheme/write.md		\
	src/arew/srfi/srfi-1.md			\
	src/arew/srfi/srfi-2.md			\
	src/arew/srfi/srfi-4.md			\
	src/arew/srfi/srfi-5.md			\
	src/arew/srfi/srfi-6.md			\
	src/arew/srfi/srfi-8.md			\
	src/arew/srfi/srfi-9.md			\
	src/arew/srfi/srfi-13.md		\
	src/arew/srfi/srfi-14.md		\
	src/arew/srfi/srfi-16.md		\
	src/arew/srfi/srfi-17.md		\
	src/arew/srfi/srfi-19.md		\
	src/arew/srfi/srfi-23.md		\
	src/arew/srfi/srfi-25.md		\
	src/arew/srfi/srfi-26.md		\
	src/arew/srfi/srfi-28.md		\
	src/arew/srfi/srfi-29.md		\
	src/arew/srfi/srfi-31.md		\
	src/arew/srfi/srfi-34.md		\
	src/arew/srfi/srfi-35.md		\
	src/arew/srfi/srfi-37.md		\
	src/arew/srfi/srfi-38.md		\
	src/arew/srfi/srfi-39.md		\
	src/arew/srfi/srfi-41.md		\
	src/arew/srfi/srfi-42.md		\
	src/arew/srfi/srfi-43.md		\
	src/arew/srfi/srfi-45.md		\
	src/arew/srfi/srfi-48.md		\
	src/arew/srfi/srfi-51.md		\
	src/arew/srfi/srfi-54.md		\
	src/arew/srfi/srfi-60.md		\
	src/arew/srfi/srfi-61.md		\
	src/arew/srfi/srfi-67.md		\
	src/arew/srfi/srfi-69.md		\
	src/arew/srfi/srfi-98.md		\
	src/arew/srfi/srfi-99.md		\
	src/arew/srfi/srfi-101.md		\
	src/arew/srfi/srfi-111.md		\
	src/arew/srfi/srfi-113.md		\
	src/arew/srfi/srfi-115.md		\
	src/arew/srfi/srfi-116.md		\
	src/arew/srfi/srfi-117.md		\
	src/arew/srfi/srfi-124.md		\
	src/arew/srfi/srfi-125.md		\
	src/arew/srfi/srfi-127.md		\
	src/arew/srfi/srfi-128.md		\
	src/arew/srfi/srfi-132.md		\
	src/arew/srfi/srfi-133.md		\
	src/arew/srfi/srfi-134.md		\
	src/arew/srfi/srfi-135.md		\
	src/arew/srfi/srfi-141.md		\
	src/arew/srfi/srfi-143.md		\
	src/arew/srfi/srfi-144.md		\
	src/arew/srfi/srfi-145.md		\
	src/arew/srfi/srfi-146.md		\
	src/arew/srfi/srfi-151.md		\
	src/arew/srfi/srfi-158.md		\
	src/arew/srfi/srfi-167.md		\
	src/arew/srfi/srfi-167/pack.md		\
	src/arew/srfi/srfi-167/engine.md	\
	src/arew/srfi/srfi-167/memory.md	\
	src/arew/srfi/srfi-173.md		\
	src/arew/stream.md			\
	src/arew/data/json.md			\
	src/arew/data/parser/combinator.md	\
	src/arew/data/base/lsm.md		\
	src/arew/network/socket.md		\

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

local/lib:
	mkdir -p local/lib

yxml: local/lib
	cd submodules/yxml && make
	cd submodules/yxml && gcc -shared -o libyxml.so yxml.o
	cp submodules/yxml/libyxml.so local/lib/

sqlite-lsm: local/lib
	cp patches/MakefileLSM submodules/sqlite/
	cd submodules/sqlite/ && make -f MakefileLSM lsm.so
	cp submodules/sqlite/lsm.so local/lib

termbox: local/lib
	cd submodules/termbox-truecolor/ && ./waf configure
	cd submodules/termbox-truecolor/ && ./waf
	cp submodules/termbox-truecolor/build/src/libtermbox.so local/lib/

init: sqlite-lsm yxml termbox

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
	./venv scheme --program make-check.scm

todo: ## Things that should be done
	@grep -nR --color=always TODO src/

xxx: ## Things that require attention
	@grep -nR --color=always --before-context=2  --after-context=2 XXX src/

clean: ## Remove useless files...
	rm arew-scheme.*
