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

.PHONY: clean-coverage
clean-coverage:
	rm -rf _coverage *.coverage
	rm -f coverage.json

.PHONY: clean
clean: clean-bs clean-native clean-docs clean-coverage

.PHONY: build-bs
build-bs:
	yarn bsb -make-world

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
	cat bastet/src/index.mld.template | \
		sed -e 's/{{:/{ {:/g' | \
		dune exec examples/docs_template.exe | \
		sed -e 's/{ {:/{{:/g' > bastet/src/index.mld

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

.PHONY: bisect-bs
bisect-bs:
	BISECT_ENABLE=yes make test-bs

.PHONY: bisect
bisect:
	BISECT_ENABLE=yes make test

.PHONY: bisect-html
bisect-html: bisect
	bisect-ppx-report html

.PHONY: coveralls-json
coveralls-json: bisect
	bisect-ppx-report coveralls --repo-token ${COVERALLS_TOKEN} coverage.json

.PHONY: coveralls-send
coveralls-send:
	curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs

.PHONY: coveralls
coveralls: coveralls-json coveralls-send

.PHONY: watch-native
watch-native:
	dune build @all -w

.PHONY: watch-bs
watch-bs:
	yarn bsb -make-world -w

.PHONY: watch-test-bs
watch-test-bs:
	yarn run watch-test

.PHONY: watch-test-native
watch-test:
	dune runtest --no-buffer -w

.PHONY: utop
utop:
	dune utop .

.PHONY: remove-switch
remove-switch:
	opam switch remove -y .

.PHONY: dev-tools
dev-tools:
	opam install -y merlin ocamlformat utop

.PHONY: 4.06-switch
4.06-switch: remove-switch
	opam switch create -y . 4.06.1 -t -d
	make dev-tools

.PHONY: default-switch
default-switch: remove-switch
	opam switch create -y . -t -d
	make dev-tools
	eval $(opam env)
