;; -*- Mode: Common-Lisp -*-

(in-package "CSS-PARSER")

(def-suite css-parser :description "Testing for the css parser.")

(in-suite css-parser)

(test zero-test
  "The simplest test"
  (is (eq t t)))

(defun p (x y)
  (equal (css-parser::parse-string x) y))

(test misc-selectors
  "Test various CSS selectors"
  (is-true (p "foo{bar:2}"
              '(stylesheet (css-rule (element "foo") -> ("bar" 2)))))
  (is (p "dinner { bar : 23; cat: 3in }
breakfast { bar : +23; bar : -23cm; bar : +23in;}
food { dogs : 23hz  }"
      '(stylesheet
        (css-rule (element "dinner") ->
         ("bar" 23) ("cat" (IN 3)))
        (css-rule (element "breakfast") ->
         ("bar" (+ 23)) ("bar" (- (CM 23))) ("bar" (+ (IN 23))))
        (css-rule (element "food") -> 
         ("dogs" (HZ 23))))))
  (is (p ".fo { b: 1; }"
         '(stylesheet (css-rule (class "fo") -> ("b" 1)))))
  (is (p " #fo { b: 1;}"
         '(stylesheet (css-rule (id "fo") -> ("b" 1)))))
  (is (p "e,f,.c,#i,:h{b:1}"
         '(stylesheet (css-rule (or (element "e") (element "f") (class "c") (id "i") (pseudo "h")) -> ("b" 1)))))
  (is (p "e:h{b:1}"
         '(stylesheet (css-rule (path (element "e") (pseudo "h")) -> ("b" 1)))))
  (is (p "e {b:1}"
         '(stylesheet (css-rule (element "e") -> ("b" 1)))))

  (is (p " e f{b:1}"
         '(stylesheet (css-rule (decendent (element "e") (element "f")) -> ("b" 1)))))
  (is (p "e+f {b:1}"
         '(stylesheet (css-rule (adjacent (element "e") (element "f")) -> ("b" 1)))))
  (is (p " e>f {b:1}"
         '(stylesheet (css-rule (child (element "e") (element "f")) -> ("b" 1)))))
  (is (p "e,f{b:1}"
         '(stylesheet (css-rule (or (element "e") (element "f")) -> ("b" 1))))))

(test misc-property-values
  "test various property values"
  (is (p "#i{b:1}" '(stylesheet (css-rule (id "i") -> ("b" 1)))))
  (is (p "#i{b:1.0}" '(stylesheet (css-rule (id "i") -> ("b" 1.0)))))
  (is (p "#i{b:0.1}" '(stylesheet (css-rule (id "i") -> ("b" 0.1)))))
  (is (p "#i{b:\"a\"}" '(stylesheet (css-rule (id "i") -> ("b" (string "a"))))))
  (is (p "#i{b:a}" '(stylesheet (css-rule (id "i") -> ("b" "a")))))
  (is (p "#i{b:1em}" '(stylesheet (css-rule (id "i") -> ("b" (em 1))))))
  (is (p "#i{b:1in}" '(stylesheet (css-rule (id "i") -> ("b" (in 1))))))
  (is (p "#i{b:1hz}" '(stylesheet (css-rule (id "i") -> ("b" (hz 1))))))
  (is (p "#i{b:1px}" '(stylesheet (css-rule (id "i") -> ("b" (px 1))))))
  (is (p "#i{b:1pt}" '(stylesheet (css-rule (id "i") -> ("b" (pt 1))))))

  )
