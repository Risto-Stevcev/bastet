# Build notes

## Testing GH actions locally

- Currently to test locally, a local version of `setup-ocaml` is setup with sandboxing disabled:
  git@github.com:avsm/setup-ocaml.git
  https://github.com/avsm/setup-ocaml/issues/6

  The `opam init` commands get passed a `--disable-sandboxing` flag (`dist/index.js`)

- Steps to use the local version:

  1. Clone and modify `avsm/setup-ocaml` into `.github/actions` and disable sandboxing
  2. Set `uses` in the GH workflow file to use the local version in `.github/actions`
  3. Uncomment the local `setup-ocaml` folder from `.gitignore` so that `act` can copy it:
     `# .github/actions/setup-ocaml/`
  4. Only run the ubuntu-latest matrix
