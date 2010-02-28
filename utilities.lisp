;; -*- Mode: Common-Lisp -*-

(in-package "CSS-PARSER")

(defvar *home-directory* nil)

(eval-when (:load-toplevel)
  (setf *home-directory* (pathname-directory *load-pathname*)))

(defun file-of-system (system filename)
  (make-pathname :defaults filename :directory (pathname-directory (asdf:component-relative-pathname (asdf:find-system system)))))

(defun css-parser-pathname ()
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
  "Re-create parser.lisp from css.peg"
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

(defvar *lisp-extension* "lisp")

(defparameter *compiled-file-extension*
  #+ccl (pathname-type CCL:*.FASL-PATHNAME*)
  #+sbcl SB-FASL:*FASL-FILE-TYPE*
  #-(or ccl sbcl) (error "Unrecognized lisp implementation."))

(defun parser-fasl-compile-as-necessary ()
  (let* ((lisp-pathname (css-parser-pathname))
         (fasl-pathname (make-pathname :defaults (css-parser-pathname) :type *compiled-file-extension*))
         (ldate (file-write-date lisp-pathname))
         (fdate? (file-write-date fasl-pathname)))
    (unless (and fdate? (< ldate fdate?))
      (compile-file lisp-pathname))
    fasl-pathname))

(defun run-parser (file)
  "Parse the css file given, return sexpr or error."
  (metapeg:parse file (parser-fasl-compile-as-necessary)))

(defun parse-string (str)
  "Parse the string, presumably cascading style sheet, or error"
  (metapeg:parse-string str (parser-fasl-compile-as-necessary)))



