(lang dune 3.17)

(name file-rewriter)

(generate_opam_files)

(license Apache-2.0)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/file-rewriter))

(documentation "https://mbarbin.github.io/file-rewriter/")

(implicit_transitive_deps false)

(package
 (name file-rewriter)
 (synopsis "Apply small rewrites to tweak or refactor your files")
 (depends
  (ocaml
   (>= 5.2))
  (fpath
   (>= 0.7.3))
  (loc
   (>= 0.2.0))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))))

(package
 (name sexps-rewriter)
 (synopsis "A specialized file-rewriter for applying rewrites to sexp files")
 (depends
  (ocaml
   (>= 5.2))
  (file-rewriter
   (= :version))
  (fpath
   (>= 0.7.3))
  (loc
   (>= 0.2.0))
  (parsexp
   (and
    (>= v0.17)
    (< v0.18)))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))))

(package
 (name file-rewriter-tests)
 (synopsis "Tests for file-rewriter")
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
  (expect_test_helpers_core
   (and
    (>= v0.17)
    (< v0.18)))
  (file-rewriter
   (= :version))
  (fpath
   (>= 0.7.3))
  (loc
   (>= 0.2.0))
  (ocamldiff
   (>= 1.2))
  (parsexp
   (and
    (>= v0.17)
    (< v0.18)))
  (pp
   (>= 2.0.0))
  (pp-log
   (>= 0.0.8))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_expect
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
  (ppxlib
   (>= 0.33))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))
  (sexps-rewriter
   (= :version))
  (stdio
   (and
    (>= v0.17)
    (< v0.18)))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
