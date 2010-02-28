;; -*- Mode: Common-Lisp -*-

(in-package "CSS-PARSER")

(defvar *home-directory* nil)

(eval-when (:load-toplevel)
  (setf *home-directory* (pathname-directory *load-pathname*)))

(defun file-of-system (system filename)
  (make-pathname :defaults filename :directory (pathname-directory (asdf:component-relative-pathname (asdf:find-system system)))))

(defun css-parser-file ()
  (file-of-system 'css-parser "parser.lisp"))

(defun collect-leaves (tree)
  (typecase tree
    (list (loop for i in tree nconc (collect-leaves i)))
    (otherwise (list tree))))

(defun intern-ident (data)
 (coerce (collect-leaves data) 'string))

(defun make-number (data)
  (read-from-string (coerce (collect-leaves data) 'string)))

(defun build-expr (data)
  ;; expr <- term ( operator? term )* { (build-expr data) }
  (let ((left (first data))
        (remainder? (second data)))
    (loop
       with result = left
       as last-op = nil then op2
       for (op right) in remainder?
       as op2 = (if (eq op 'metapeg::optional)
                    'list
                    op)
       do (setf result
                (if (eq op2 last-op)
                    `(,op2 ,@(cdr result) ,right)
                    `(,op2 ,result ,right)))
       finally (return result))))

(defun make-hex (digits)
  `(color ,(coerce digits 'string)))


#+nil
(defun 2nil (data)
  (declare (ignore data))
  nil)

(defvar *trace-reducing* nil)

(defun trace-reducing (name action lambda data)
  (when *trace-reducing*
    (format t "~&Reducing ~S ~A ~S" name action data))
  (let ((result (funcall lambda data)))
    (when *trace-reducing*
      (format t " --> ~S" result))
    result))
  


(defun build-parser ()
  (multiple-value-bind (pegs actions)
      (metapeg:parse (file-of-system 'css-parser "css.peg") (file-of-system 'metapeg "metapeg.lisp"))
    (with-open-file (*standard-output* (file-of-system 'css-parser "parser.lisp")
                                       :direction :output
                                       :if-exists :rename-and-delete)
      (format t ";; -*- Mode: Common-Lisp -*-")
      (format t "~2&;; This file is machine generated")
      (let ((*package* (find-package "METAPEG"))
            (*print-case* :downcase))
        (format t "~3&;;; Parsing Functions")
        (loop 
           for peg in pegs
           do
           (format t "~2&")
           (pprint peg))
        (format t "~3&;;; Actions")
        (loop 
           for (name action) in actions
           as stms = (let ((*package* (symbol-package 'this-package)))
                       (read-from-string (format nil "(~A)" action)))
           do
           (let ((*package* (find-package "METAPEG")))
             (format t "~&")
             (pprint 
              `(defun ,name (data)
                 (trace-reducing ',name 
                                 ,action 
                                 #'(lambda (data)
                                     (macrolet ((-> (pattern &body body)
                                                  `(progn
                                                     ;; drop the info about what action etc.
                                                     (setf (cdr (last data 2)) nil)
                                                     (match data
                                                       (,pattern  ,@body)))))
                                       ,@stms))
                                 data)))))))))



(defun run-parser (file &optional rebuild)
  (when rebuild (build-parser))
  (let* ((lisp (css-parser-file))
         (fasl (make-pathname :defaults (css-parser-file) :type "dx64fsl"))
         (ldate (file-write-date lisp))
         (fdate? (file-write-date fasl))
         (parser (if (and fdate? (< ldate fdate?)) fasl lisp)))
    (when (equal parser lisp)
      (format t "Consider compiling parser."))
    (metapeg:parse file parser)))

(defun parse-string (str)
  (handler-case (metapeg:parse-string str (css-parser-file))
    (error (e) (format t "~&Error ~A" e))))

(defun test-parser ()
  (flet ((test (a b)
           (let ((result
                  (handler-case
                      (metapeg:parse-string a (css-parser-file))
                    (error (e)
                      (format t "~&Error: ~A" e)
                      :foo))))
             (unless (equal result b)               (format t "~&Failed ~S" a)))))
    (test "foo{bar:2}"
          '(stylesheet (css-rule (element "foo") -> ("bar" 2))))
    (test "dinner { bar : 23; cat: 3in }
breakfast { bar : +23; bar : -23cm; bar : +23in;}
food { dogs : 23hz  }"
          '(stylesheet
            (css-rule (element "dinner") ->
             ("bar" 23) ("cat" (IN 3)))
            (css-rule (element "breakfast") ->
             ("bar" (+ 23)) ("bar" (- (CM 23))) ("bar" (+ (IN 23))))
            (css-rule (element "food") -> 
             ("dogs" (HZ 23)))))
    (test ".fo { b: 1; }" '(stylesheet (css-rule (class "fo") -> ("b" 1))))
    (test " #fo { b: 1;}" '(stylesheet (css-rule (id "fo") -> ("b" 1))))
    (test "e,f,.c,#i,:h{b:1}" '(stylesheet (css-rule (or (element "e") (element "f") (class "c") (id "i") (pseudo "h")) -> ("b" 1))))
    (test "e:h{b:1}" '(stylesheet (css-rule (path (element "e") (pseudo "h")) -> ("b" 1))))
    (test "e {b:1}" '(stylesheet (css-rule (element "e") -> ("b" 1))))
    :done))

