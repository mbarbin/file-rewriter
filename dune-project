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
   (>= 0.2.2))
  (sexplib0
   (>= v0.17))))

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
   (>= 0.2.2))
  (parsexp
   (>= v0.17))
  (sexplib0
   (>= v0.17))))

(package
 (name file-rewriter-tests)
 (synopsis "Tests for file-rewriter")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (>= v0.17))
  (expect_test_helpers_core
   (>= v0.17))
  (file-rewriter
   (= :version))
  (fpath
   (>= 0.7.3))
  (loc
   (>= 0.2.2))
  (parsexp
   (>= v0.17))
  (patdiff
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing-err
   (>= 0.0.16))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (= :version))
  (stdio
   (>= v0.17))))

(package
 (name file-rewriter-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (= 0.27.0))
  (base
   (>= v0.17))
  (bisect_ppx
   (>= 2.8.3))
  (expect_test_helpers_core
   (>= v0.17))
  (file-rewriter
   (= :version))
  (file-rewriter-tests
   (= :version))
  (fpath
   (>= 0.7.3))
  (loc
   (>= 0.2.2))
  (parsexp
   (>= v0.17))
  (patdiff
   (>= v0.17))
  (pp
   (>= 2.0.0))
  (pplumbing-err
   (>= 0.0.16))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (sexplib0
   (>= v0.17))
  (sexps-rewriter
   (= :version))
  (stdio
   (>= v0.17))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
