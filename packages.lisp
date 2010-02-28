;; -*- Mode: Common-Lisp -*-

(defpackage "CSS-PARSER"
  (:use "COMMON-LISP" "FARE-MATCHER" #+5am "FIVEAM")
  (:export "PARSE" 
           "PARSE-STRING"
           #+5am "TESTS"
           ))
