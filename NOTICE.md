# License

The code, tests and libraries contained in the file-rewriter repository are released under the terms of the `Apache-2.0` license.

## License, copyright & notices

- **COPYING.HEADER** contains the copyright and license notices. It is added as a header to every file in the project.

- **LICENSE** contains a copy of the full [Apache-2.0 license](https://www.apache.org/licenses/LICENSE-2.0.txt)

- **NOTICE.md** (this file) documents the project licensing.

## Third party licenses

Under `third-party-license/` we include the license of software used as vendored code.

## Gazagnaire ocaml-merge3 (Myers diff)

The Myers shortest-edit-script computation in `src/myers/merge3.ml` is vendored
from [ocaml-merge3](https://tangled.org/gazagnaire.org/ocaml-merge3) by Thomas
Gazagnaire (released under `ISC`). Only the pure diff computation is vendored;
the parts unused by this project are not included. The exact provenance and list
of changes are documented at the top of `src/myers/merge3.ml` and in
`src/myers/vendor.json`.

A copy of the license file for ocaml-merge3 is located under
`third-party-license/gazagnaire/ocaml-merge3/LICENSE`.

## Windtrap (unified-diff renderer)

The unified-diff renderer in `src/myers/myers.ml` is vendored from
[windtrap](https://github.com/invariant-hq/windtrap) by Invariant Systems
(released under `ISC`). The exact provenance and list of changes are documented
at the top of `src/myers/myers.ml` and in `src/myers/vendor.json`.

A copy of the license file for windtrap is located under
`third-party-license/invariant-hq/windtrap/LICENSE`.
