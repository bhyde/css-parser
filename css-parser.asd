;; -*- mode: lisp -*-

(defsystem css-parser
  :description "A parser for css files"
  :VERSION "0.1"
  :author "Ben Hyde"
  :license "http://www.apache.org/licenses/LICENSE-2.0"
  :depends-on (metapeg fare-matcher)
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "parser")))
