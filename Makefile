SOURCES = 					\
	README.md				\
	src/scheme/base.md			\
	src/scheme/bytevector.md		\
	src/scheme/case-lambda.md		\
	src/scheme/char.md			\
	src/scheme/charset.md		\
	src/scheme/complex.md		\
	src/scheme/cxr.md			\
	src/scheme/eval.md			\
	src/scheme/file.md			\
	src/scheme/inexact.md		\
	src/scheme/lazy.md			\
	src/scheme/load.md			\
	src/scheme/process-context.md	\
	src/scheme/r5rs.md			\
	src/scheme/read.md			\
	src/scheme/repl.md			\
	src/scheme/time.md			\
	src/scheme/write.md		\
	src/srfi/srfi-1.md			\
	src/srfi/srfi-2.md			\
	src/srfi/srfi-4.md			\
	src/srfi/srfi-5.md			\
	src/srfi/srfi-6.md			\
	src/srfi/srfi-8.md			\
	src/srfi/srfi-9.md			\
	src/srfi/srfi-13.md		\
	src/srfi/srfi-14.md		\
	src/srfi/srfi-16.md		\
	src/srfi/srfi-17.md		\
	src/srfi/srfi-19.md		\
	src/srfi/srfi-23.md		\
	src/srfi/srfi-25.md		\
	src/srfi/srfi-26.md		\
	src/srfi/srfi-28.md		\
	src/srfi/srfi-29.md		\
	src/srfi/srfi-31.md		\
	src/srfi/srfi-34.md		\
	src/srfi/srfi-35.md		\
	src/srfi/srfi-37.md		\
	src/srfi/srfi-38.md		\
	src/srfi/srfi-39.md		\
	src/srfi/srfi-41.md		\
	src/srfi/srfi-42.md		\
	src/srfi/srfi-43.md		\
	src/srfi/srfi-45.md		\
	src/srfi/srfi-48.md		\
	src/srfi/srfi-51.md		\
	src/srfi/srfi-54.md		\
	src/srfi/srfi-60.md		\
	src/srfi/srfi-61.md		\
	src/srfi/srfi-67.md		\
	src/srfi/srfi-69.md		\
	src/srfi/srfi-98.md		\
	src/srfi/srfi-99.md		\
	src/srfi/srfi-101.md		\
	src/srfi/srfi-111.md		\
	src/srfi/srfi-113.md		\
	src/srfi/srfi-115.md		\
	src/srfi/srfi-116.md		\
	src/srfi/srfi-117.md		\
	src/srfi/srfi-124.md		\
	src/srfi/srfi-125.md		\
	src/srfi/srfi-127.md		\
	src/srfi/srfi-128.md		\
	src/srfi/srfi-132.md		\
	src/srfi/srfi-133.md		\
	src/srfi/srfi-134.md		\
	src/srfi/srfi-135.md		\
	src/srfi/srfi-141.md		\
	src/srfi/srfi-143.md		\
	src/srfi/srfi-144.md		\
	src/srfi/srfi-145.md		\
	src/srfi/srfi-146.md		\
	src/srfi/srfi-151.md		\
	src/srfi/srfi-158.md		\
	src/srfi/srfi-167.md		\
	src/srfi/srfi-167/pack.md		\
	src/srfi/srfi-167/engine.md	\
	src/srfi/srfi-167/memory.md	\
	src/srfi/srfi-173.md		\
	src/arew/data/base/okvslite.md	\

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

local/lib:
	mkdir -p local/lib

yxml: local/lib
	cd submodules/yxml && make
	cd submodules/yxml && gcc -shared -o libyxml.so yxml.o
	cp submodules/yxml/libyxml.so local/lib/

okvslite: local/lib
	cp patches/MakefileLSM submodules/sqlite/
	cd submodules/sqlite/ && make -f MakefileLSM lsm.so
	cp submodules/sqlite/lsm.so local/lib

termbox: local/lib
	cd submodules/termbox/ && ./waf configure
	cd submodules/termbox/ && ./waf
	cp submodules/termbox/build/src/libtermbox.so local/lib/

init: okvslite yxml termbox

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
	./venv scheme --program src/arew.scm test src/

todo: ## Things that should be done
	@grep -nR --color=always TODO src/

xxx: ## Things that require attention
	@grep -nR --color=always --before-context=2  --after-context=2 XXX src/

clean: ## Remove useless files...
	rm arew-scheme.*
