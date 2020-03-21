all: build

.PHONY: clean-bs
clean-bs:
	bsb -clean-world

.PHONY: clean-native
clean-native:
	dune clean

.PHONY: clean-docs
clean-docs:
	rm -rf docs/**

.PHONY: clean
clean: clean-bs clean-native clean-docs

.PHONY: build-bs
build-bs:
	bsb -make-world

.PHONY: build-native
build-native:
	dune build @all

.PHONY: build
build: build-bs build-native

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: docs-template
docs-template: test-native
	cat ocaml_abstract/src/index.mld.template | \
		sed -e 's/{{:/{ {:/g' | \
		dune exec examples/docs_template.exe | \
		sed -e 's/{ {:/{{:/g' > ocaml_abstract/src/index.mld

.PHONY: docs
docs: clean-docs docs-template
	dune build @doc

.PHONY: copy-docs
copy-docs: docs
	cp -r _build/default/_doc/_html/** docs/

.PHONY: open-docs
open-docs: copy-docs
	xdg-open docs/index.html

.PHONY: test-bs
test-bs: build-bs
	yarn test

.PHONY: test-native
test-native: build-native
	dune runtest --no-buffer

.PHONY: test
test: test-bs test-native

.PHONY: watch-native
watch-native:
	dune build @all -w

.PHONY: watch-bs
watch-bs:
	bsb -make-world -w

.PHONY: watch-test-bs
watch-test-bs:
	yarn run watch-test
	
.PHONY: watch-test-native
watch-test:
	dune runtest --no-buffer -w

.PHONY: utop
utop:
	dune utop .
