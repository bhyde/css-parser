;; -*- Mode: Common-Lisp -*-

;; This file is machine generated


;;; Parsing Functions


(in-package :metapeg)


(declaim (optimize (speed 3) (safety 0) (debug 0)))


(defun generated-parser ()
  (let ((*context* (make-instance 'context :start-index 0)))
    (funcall (|parse_program|) 0)))


(defun |parse_program| ()
  (lambda (offset)
    (build-parser-function
      "program"
      (seq (many (|parse_S|))
           (|parse_stylesheet|)
           (list 'action nil 'metapeg-action385)))))


(defun |parse_digit| ()
  (lambda (offset)
    (build-parser-function
      "digit"
      (match-char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))))


(defun |parse_nmstart| ()
  (lambda (offset)
    (build-parser-function
      "nmstart"
      (match-char '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O
                    #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d
                    #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
                    #\t #\u #\v #\w #\x #\y #\z #\_)))))


(defun |parse_nmchar| ()
  (lambda (offset)
    (build-parser-function
      "nmchar"
      (either (|parse_nmstart|)
              (match-char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-))))))


(defun |parse_minus| ()
  (lambda (offset) (build-parser-function "minus" (match-string "-"))))


(defun |parse_IDENT| ()
  (lambda (offset)
    (build-parser-function
      "IDENT"
      (seq (|parse_nmstart|)
           (many (|parse_nmchar|))
           (list 'action nil 'metapeg-action386)))))


(defun |parse_NUMBER| ()
  (lambda (offset)
    (build-parser-function
      "NUMBER"
      (either (seq (many (|parse_digit|))
                   (match-string ".")
                   (many1 (|parse_digit|))
                   (list 'action nil 'metapeg-action387))
              (seq (many1 (|parse_digit|))
                   (list 'action nil 'metapeg-action388))))))


(defun |parse_ws| ()
  (lambda (offset) (build-parser-function "ws" (match-char '(#\  #\Tab)))))


(defun |parse_nl| ()
  (lambda (offset) (build-parser-function "nl" (match-char '(#\Newline)))))


(defun |parse_comment| ()
  (lambda (offset)
    (build-parser-function
      "comment"
      (seq (match-string "/*")
           (many (seq (negate (match-string "*/")) (match-any-char 'dummy)))
           (match-string "*/")))))


(defun |parse_ws_or_nl| ()
  (lambda (offset)
    (build-parser-function "ws_or_nl" (either (|parse_ws|) (|parse_nl|)))))


(defun |parse_S| ()
  (lambda (offset)
    (build-parser-function "S" (either (|parse_ws_or_nl|) (|parse_comment|)))))


(defun |parse_stylesheet| ()
  (lambda (offset) (build-parser-function "stylesheet" (many (|parse_stmt|)))))


(defun |parse_stmt| ()
  (lambda (offset)
    (build-parser-function "stmt" (either (|parse_rule|) (|parse_media|)))))


(defun |parse_rule| ()
  (lambda (offset)
    (build-parser-function
      "rule"
      (seq (|parse_rule_lhs|)
           (|parse_rule_rhs|)
           (list 'action nil 'metapeg-action389)))))


(defun |parse_rule_lhs| ()
  (lambda (offset)
    (build-parser-function
      "rule_lhs"
      (seq (|parse_selector|)
           (many (|parse_more_selector|))
           (list 'action nil 'metapeg-action390)))))


(defun |parse_more_selector| ()
  (lambda (offset)
    (build-parser-function
      "more_selector"
      (seq (match-string ",")
           (many (|parse_S|))
           (|parse_selector|)
           (list 'action nil 'metapeg-action391)))))


(defun |parse_rule_rhs| ()
  (lambda (offset)
    (build-parser-function
      "rule_rhs"
      (seq (match-string "{")
           (|parse_decls|)
           (optional (match-string ";"))
           (many (|parse_S|))
           (match-string "}")
           (many (|parse_S|))
           (list 'action nil 'metapeg-action392)))))


(defun |parse_decls| ()
  (lambda (offset)
    (build-parser-function
      "decls"
      (seq (many (|parse_S|))
           (|parse_declaration|)
           (many (|parse_more_declaration|))
           (list 'action nil 'metapeg-action393)))))


(defun |parse_more_declaration| ()
  (lambda (offset)
    (build-parser-function
      "more_declaration"
      (seq (match-string ";")
           (many (|parse_S|))
           (|parse_declaration|)
           (list 'action nil 'metapeg-action394)))))


(defun |parse_selector| ()
  (lambda (offset)
    (build-parser-function
      "selector"
      (seq (|parse_simple_selector|)
           (optional (|parse_selector_trailer|))
           (list 'action nil 'metapeg-action395)))))


(defun |parse_selector_trailer| ()
  (lambda (offset)
    (build-parser-function
      "selector_trailer"
      (either (|parse_selector_trailer_a|)
              (|parse_selector_trailer_b|)
              (|parse_selector_trailer_c|)
              (|parse_selector_trailer_d|)))))


(defun |parse_selector_trailer_a| ()
  (lambda (offset)
    (build-parser-function
      "selector_trailer_a"
      (seq (|parse_combinator|) (|parse_selector|)))))


(defun |parse_selector_trailer_b| ()
  (lambda (offset)
    (build-parser-function
      "selector_trailer_b"
      (seq (many1 (|parse_S|))
           (|parse_combinator|)
           (|parse_selector|)
           (list 'action nil 'metapeg-action396)))))


(defun |parse_selector_trailer_c| ()
  (lambda (offset)
    (build-parser-function
      "selector_trailer_c"
      (seq (many1 (|parse_S|))
           (|parse_selector|)
           (list 'action nil 'metapeg-action397)))))


(defun |parse_selector_trailer_d| ()
  (lambda (offset)
    (build-parser-function
      "selector_trailer_d"
      (seq (many1 (|parse_S|)) (list 'action nil 'metapeg-action398)))))


(defun |parse_simple_selector| ()
  (lambda (offset)
    (build-parser-function
      "simple_selector"
      (either (seq (|parse_element_name|)
                   (many (|parse_simple_selector_etc|))
                   (list 'action nil 'metapeg-action399))
              (seq (many1 (|parse_simple_selector_etc|))
                   (list 'action nil 'metapeg-action400))))))


(defun |parse_simple_selector_etc| ()
  (lambda (offset)
    (build-parser-function
      "simple_selector_etc"
      (either (|parse_id_selector|)
              (|parse_class_selector|)
              (|parse_attrib_selector|)
              (|parse_pseudo_selector|)))))


(defun |parse_element_name| ()
  (lambda (offset)
    (build-parser-function
      "element_name"
      (either (|parse_element_selector|) (|parse_wild_element_selector|)))))


(defun |parse_element_selector| ()
  (lambda (offset)
    (build-parser-function
      "element_selector"
      (seq (|parse_IDENT|) (list 'action nil 'metapeg-action401)))))


(defun |parse_wild_element_selector| ()
  (lambda (offset)
    (build-parser-function
      "wild_element_selector"
      (seq (match-string "*") (list 'action nil 'metapeg-action402)))))


(defun |parse_id_selector| ()
  (lambda (offset)
    (build-parser-function
      "id_selector"
      (seq (match-string "#")
           (|parse_IDENT|)
           (list 'action nil 'metapeg-action403)))))


(defun |parse_class_selector| ()
  (lambda (offset)
    (build-parser-function
      "class_selector"
      (seq (match-string ".")
           (|parse_IDENT|)
           (list 'action nil 'metapeg-action404)))))


(defun |parse_attrib_selector| ()
  (lambda (offset)
    (build-parser-function
      "attrib_selector"
      (seq (match-string "[")
           (many (|parse_S|))
           (|parse_IDENT|)
           (optional (seq (either (match-string "=")
                                  (match-string "~=")
                                  (match-string "|="))
                          (many (|parse_S|))
                          (either (|parse_IDENT|) (|parse_string|))
                          (many (|parse_S|))))
           (match-string "]")))))


(defun |parse_pseudo_selector| ()
  (lambda (offset)
    (build-parser-function
      "pseudo_selector"
      (seq (match-string ":")
           (|parse_IDENT|)
           (list 'action nil 'metapeg-action405)))))


(defun |parse_tbd_pseudo_selector| ()
  (lambda (offset)
    (build-parser-function
      "tbd_pseudo_selector"
      (seq (match-string ":")
           (|parse_IDENT|)
           (optional (seq (match-string "(")
                          (optional (seq (|parse_IDENT|) (many (|parse_S|))))
                          (match-string ")")))
           (list 'action nil 'metapeg-action406)))))


(defun |parse_combinator| ()
  (lambda (offset)
    (build-parser-function
      "combinator"
      (either (seq (match-string "+")
                   (many (|parse_S|))
                   (list 'action nil 'metapeg-action407))
              (seq (match-string ">")
                   (many (|parse_S|))
                   (list 'action nil 'metapeg-action408))))))


(defun |parse_declaration| ()
  (lambda (offset)
    (build-parser-function
      "declaration"
      (seq (|parse_property|)
           (match-string ":")
           (many (|parse_S|))
           (|parse_expr|)
           (optional (|parse_prio|))
           (list 'action nil 'metapeg-action409)))))


(defun |parse_property| ()
  (lambda (offset)
    (build-parser-function
      "property"
      (seq (|parse_IDENT|)
           (many (|parse_S|))
           (list 'action nil 'metapeg-action410)))))


(defun |parse_prio| ()
  (lambda (offset)
    (build-parser-function
      "prio"
      (seq (match-string "!")
           (many (|parse_S|))
           (list 'action nil 'metapeg-action411)))))


(defun |parse_expr| ()
  (lambda (offset)
    (build-parser-function
      "expr"
      (seq (|parse_term|)
           (many (seq (optional (|parse_operator|)) (|parse_term|)))
           (list 'action nil 'metapeg-action412)))))


(defun |parse_operator| ()
  (lambda (offset)
    (build-parser-function
      "operator"
      (seq (|parse_operator_a|)
           (many (|parse_S|))
           (list 'action nil 'metapeg-action413)))))


(defun |parse_operator_a| ()
  (lambda (offset)
    (build-parser-function
      "operator_a"
      (either (seq (match-string "/") (list 'action nil 'metapeg-action414))
              (seq (match-string ",") (list 'action nil 'metapeg-action415))))))


(defun |parse_term| ()
  (lambda (offset)
    (build-parser-function
      "term"
      (either (seq (|parse_unary_op|)
                   (|parse_measure|)
                   (list 'action nil 'metapeg-action416))
              (|parse_measure|)
              (|parse_other_term|)))))


(defun |parse_unary_op| ()
  (lambda (offset)
    (build-parser-function
      "unary_op"
      (either (seq (match-string "-") (list 'action nil 'metapeg-action417))
              (seq (match-string "+") (list 'action nil 'metapeg-action418))))))


(defun |parse_measure| ()
  (lambda (offset)
    (build-parser-function
      "measure"
      (seq (|parse_mess|)
           (many (|parse_S|))
           (list 'action nil 'metapeg-action419)))))


(defun |parse_mess| ()
  (lambda (offset)
    (build-parser-function
      "mess"
      (either (|parse_em_m|)
              (|parse_ex_m|)
              (|parse_px_m|)
              (|parse_cm_m|)
              (|parse_mm_m|)
              (|parse_in_m|)
              (|parse_pt_m|)
              (|parse_pc_m|)
              (|parse_deg_m|)
              (|parse_rad_m|)
              (|parse_grad_m|)
              (|parse_ms_m|)
              (|parse_s_m|)
              (|parse_hx_m|)
              (|parse_khz_m|)
              (|parse_precent_m|)
              (|parse_dim_m|)
              (|parse_raw_m|)))))


(defun |parse_em_m| ()
  (lambda (offset)
    (build-parser-function
      "em_m"
      (seq (|parse_NUMBER|)
           (match-string "em")
           (list 'action nil 'metapeg-action420)))))


(defun |parse_ex_m| ()
  (lambda (offset)
    (build-parser-function
      "ex_m"
      (seq (|parse_NUMBER|)
           (match-string "ex")
           (list 'action nil 'metapeg-action421)))))


(defun |parse_px_m| ()
  (lambda (offset)
    (build-parser-function
      "px_m"
      (seq (|parse_NUMBER|)
           (match-string "px")
           (list 'action nil 'metapeg-action422)))))


(defun |parse_cm_m| ()
  (lambda (offset)
    (build-parser-function
      "cm_m"
      (seq (|parse_NUMBER|)
           (match-string "cm")
           (list 'action nil 'metapeg-action423)))))


(defun |parse_mm_m| ()
  (lambda (offset)
    (build-parser-function
      "mm_m"
      (seq (|parse_NUMBER|)
           (match-string "mm")
           (list 'action nil 'metapeg-action424)))))


(defun |parse_in_m| ()
  (lambda (offset)
    (build-parser-function
      "in_m"
      (seq (|parse_NUMBER|)
           (match-string "in")
           (list 'action nil 'metapeg-action425)))))


(defun |parse_pt_m| ()
  (lambda (offset)
    (build-parser-function
      "pt_m"
      (seq (|parse_NUMBER|)
           (match-string "pt")
           (list 'action nil 'metapeg-action426)))))


(defun |parse_pc_m| ()
  (lambda (offset)
    (build-parser-function
      "pc_m"
      (seq (|parse_NUMBER|)
           (match-string "pc")
           (list 'action nil 'metapeg-action427)))))


(defun |parse_deg_m| ()
  (lambda (offset)
    (build-parser-function
      "deg_m"
      (seq (|parse_NUMBER|)
           (match-string "deg")
           (list 'action nil 'metapeg-action428)))))


(defun |parse_rad_m| ()
  (lambda (offset)
    (build-parser-function
      "rad_m"
      (seq (|parse_NUMBER|)
           (match-string "rad")
           (list 'action nil 'metapeg-action429)))))


(defun |parse_grad_m| ()
  (lambda (offset)
    (build-parser-function
      "grad_m"
      (seq (|parse_NUMBER|)
           (match-string "grad")
           (list 'action nil 'metapeg-action430)))))


(defun |parse_ms_m| ()
  (lambda (offset)
    (build-parser-function
      "ms_m"
      (seq (|parse_NUMBER|)
           (match-string "ms")
           (list 'action nil 'metapeg-action431)))))


(defun |parse_s_m| ()
  (lambda (offset)
    (build-parser-function
      "s_m"
      (seq (|parse_NUMBER|)
           (match-string "s")
           (list 'action nil 'metapeg-action432)))))


(defun |parse_hx_m| ()
  (lambda (offset)
    (build-parser-function
      "hx_m"
      (seq (|parse_NUMBER|)
           (match-string "hz")
           (list 'action nil 'metapeg-action433)))))


(defun |parse_khz_m| ()
  (lambda (offset)
    (build-parser-function
      "khz_m"
      (seq (|parse_NUMBER|)
           (match-string "khz")
           (list 'action nil 'metapeg-action434)))))


(defun |parse_precent_m| ()
  (lambda (offset)
    (build-parser-function
      "precent_m"
      (seq (|parse_NUMBER|)
           (match-string "%")
           (list 'action nil 'metapeg-action435)))))


(defun |parse_dim_m| ()
  (lambda (offset)
    (build-parser-function
      "dim_m"
      (seq (|parse_NUMBER|)
           (|parse_IDENT|)
           (list 'action nil 'metapeg-action436)))))


(defun |parse_raw_m| ()
  (lambda (offset) (build-parser-function "raw_m" (|parse_NUMBER|))))


(defun |parse_other_term| ()
  (lambda (offset)
    (build-parser-function
      "other_term"
      (either (seq (|parse_string|)
                   (many (|parse_S|))
                   (list 'action nil 'metapeg-action437))
              (seq (|parse_URI|)
                   (many (|parse_S|))
                   (list 'action nil 'metapeg-action438))
              (seq (|parse_IDENT|)
                   (many (|parse_S|))
                   (list 'action nil 'metapeg-action439))
              (seq (|parse_hexcolor|)
                   (many (|parse_S|))
                   (list 'action nil 'metapeg-action440))
              (|parse_function|)))))


(defun |parse_function| ()
  (lambda (offset)
    (build-parser-function
      "function"
      (seq (|parse_IDENT|)
           (match-string "(")
           (many (|parse_S|))
           (|parse_expr|)
           (match-string ")")
           (many (|parse_S|))
           (list 'action nil 'metapeg-action441)))))


(defun |parse_string| ()
  (lambda (offset)
    (build-parser-function
      "string"
      (seq (match-char '(#\"))
           (many (seq (negate (match-char '(#\"))) (match-any-char 'dummy)))
           (match-char '(#\"))
           (list 'action nil 'metapeg-action442)))))


(defun |parse_URI| ()
  (lambda (offset)
    (build-parser-function
      "URI"
      (seq (match-string "url(")
           (many (seq (negate (match-string ")")) (match-any-char 'dummy)))
           (match-string ")")
           (list 'action nil 'metapeg-action443)))))


(defun |parse_hexcolor| ()
  (lambda (offset)
    (build-parser-function
      "hexcolor"
      (either (seq (match-string "#")
                   (|parse_hex|)
                   (|parse_hex|)
                   (|parse_hex|)
                   (|parse_hex|)
                   (|parse_hex|)
                   (|parse_hex|)
                   (list 'action nil 'metapeg-action444))
              (seq (match-string "#")
                   (|parse_hex|)
                   (|parse_hex|)
                   (|parse_hex|)
                   (list 'action nil 'metapeg-action445))))))


(defun |parse_hex| ()
  (lambda (offset)
    (build-parser-function
      "hex"
      (match-char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e
                    #\f #\A #\B #\C #\D #\E #\F)))))


(defun |parse_medium| ()
  (lambda (offset)
    (build-parser-function
      "medium"
      (seq (|parse_IDENT|)
           (many (|parse_S|))
           (list 'action nil 'metapeg-action446)))))


(defun |parse_medium-list| ()
  (lambda (offset)
    (build-parser-function
      "medium-list"
      (seq (|parse_medium|)
           (many (seq (match-string ",") (many (|parse_S|)) (|parse_medium|)))
           (list 'action nil 'metapeg-action447)))))


(defun |parse_media| ()
  (lambda (offset)
    (build-parser-function
      "media"
      (seq (match-string "@media")
           (many (|parse_S|))
           (|parse_medium|)
           (match-string "{")
           (many (|parse_S|))
           (|parse_stylesheet|)
           (match-string "}")
           (list 'action nil 'metapeg-action448)))))


;;; Actions

(defun metapeg-action448 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action448
    " (-> `(,a,b,c,d,e,f,g)  `(media (,@c) ,@(rest f))) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a
                  (list* css-parser::b
                         (list* css-parser::c
                                (list* css-parser::d
                                       (list*
                                        css-parser::e
                                        (list*
                                         css-parser::f
                                         (list css-parser::g)))))))
           (list* 'css-parser::media
                  (list* css-parser::c (rest css-parser::f))))))
    css-parser::data))

(defun metapeg-action447 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action447
    "(-> `(,a ,b) `(,@a ,@(mapcar #'third b)))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (append css-parser::a (mapcar #'third css-parser::b)))))
    css-parser::data))

(defun metapeg-action446 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action446
    " (-> `(,a ,b) a) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action445 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action445
    " (make-hex (subseq data 1 4)) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::make-hex (subseq css-parser::data 1 4))))
    css-parser::data))

(defun metapeg-action444 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action444
    " (make-hex (subseq data 1 7)) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::make-hex (subseq css-parser::data 1 7))))
    css-parser::data))

(defun metapeg-action443 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action443
    "(-> `(,a ,b ,c) `(url ,(coerce (mapcar #'second b) 'string)))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           (list* 'css-parser::url
                  (list (coerce (mapcar #'second css-parser::b) 'string))))))
    css-parser::data))

(defun metapeg-action442 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action442
    "(-> `(,a ,b ,c) `(string ,(coerce (mapcar #'second b) 'string)))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           (list* 'string
                  (list (coerce (mapcar #'second css-parser::b) 'string))))))
    css-parser::data))

(defun metapeg-action441 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action441
    "(-> `(,a ,b ,c ,d ,e ,f) `(funcall ,a ,d))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a
                  (list* css-parser::b
                         (list* css-parser::c
                                (list* css-parser::d
                                       (list*
                                        css-parser::e
                                        (list css-parser::f))))))
           (list* 'funcall (list* css-parser::a (list css-parser::d))))))
    css-parser::data))

(defun metapeg-action440 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action440
    "(-> `(,a ,b) a)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action439 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action439
    "(-> `(,a ,b) a)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action438 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action438
    "(-> `(,a ,b) a)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action437 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action437
    "(-> `(,a ,b) a)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action436 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action436
    "(-> `(,a ,b) `(dimension ,a ,b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::dimension
                  (list* css-parser::a (list css-parser::b))))))
    css-parser::data))

(defun metapeg-action435 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action435
    "(-> `(,a ,b) `(percent ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::percent (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action434 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action434
    "(-> `(,a ,b) `(khz ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::khz (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action433 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action433
    "(-> `(,a ,b) `(hz ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::hz (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action432 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action432
    "(-> `(,a ,b) `(s ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::s (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action431 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action431
    "(-> `(,a ,b) `(ms ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::ms (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action430 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action430
    "(-> `(,a ,b) `(grad ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::grad (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action429 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action429
    "(-> `(,a ,b) `(rad ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::rad (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action428 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action428
    "(-> `(,a ,b) `(deg ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::deg (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action427 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action427
    "(-> `(,a ,b) `(pc ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::pc (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action426 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action426
    "(-> `(,a ,b) `(pt ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::pt (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action425 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action425
    "(-> `(,a ,b) `(in ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::in (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action424 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action424
    "(-> `(,a ,b) `(mm ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::mm (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action423 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action423
    "(-> `(,a ,b) `(cm ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::cm (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action422 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action422
    "(-> `(,a ,b) `(px ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::px (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action421 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action421
    "(-> `(,a ,b) `(ex ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::ex (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action420 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action420
    "(-> `(,a ,b) `(em ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::em (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action419 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action419
    " (-> `(,a ,b) a) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action418 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action418
    "(-> `(,a) '+)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a) '+)))
    css-parser::data))

(defun metapeg-action417 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action417
    "(-> `(,a) '-)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a) '-)))
    css-parser::data))

(defun metapeg-action416 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action416
    "(-> `(,a ,b) `(,a ,b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* css-parser::a (list css-parser::b)))))
    css-parser::data))

(defun metapeg-action415 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action415
    "(-> `(,a) 'comma)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a) 'css-parser::comma)))
    css-parser::data))

(defun metapeg-action414 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action414
    "(-> `(,a) 'divide)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a) 'css-parser::divide)))
    css-parser::data))

(defun metapeg-action413 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action413
    "(-> `(,a ,b) a)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action412 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action412
    "(-> `(,a ,b) (build-expr `(,a ,b)))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (css-parser::build-expr (list* css-parser::a
                                          (list css-parser::b))))))
    css-parser::data))

(defun metapeg-action411 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action411
    "(-> `(,a ,b) t)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b)) t)))
    css-parser::data))

(defun metapeg-action410 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action410
    " (-> `(,a ,b) a) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           css-parser::a)))
    css-parser::data))

(defun metapeg-action409 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action409
    "(-> `(,a ,b ,c ,d ,e) `(,a ,(if (eq 'metapeg::optional e) d `(priority ,d))))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a
                  (list* css-parser::b
                         (list* css-parser::c
                                (list* css-parser::d (list css-parser::e)))))
           (list* css-parser::a
                  (list (if (eq 'optional css-parser::e)
                            css-parser::d
                            (list* 'css-parser::priority
                                   (list css-parser::d))))))))
    css-parser::data))

(defun metapeg-action408 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action408
    "(-> `(,a ,b) 'child)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           'css-parser::child)))
    css-parser::data))

(defun metapeg-action407 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action407
    "(-> `(,a ,b) 'adjacent)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           'css-parser::adjacent)))
    css-parser::data))

(defun metapeg-action406 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action406
    "(-> `(,a ,b ,c ,d ,e) 'pseudo_selector_tbd)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a
                  (list* css-parser::b
                         (list* css-parser::c
                                (list* css-parser::d (list css-parser::e)))))
           'css-parser::pseudo_selector_tbd)))
    css-parser::data))

(defun metapeg-action405 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action405
    "(-> `(,a ,b) `(pseudo ,b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::pseudo (list css-parser::b)))))
    css-parser::data))

(defun metapeg-action404 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action404
    "(-> `(,a ,b) `(class ,b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'class (list css-parser::b)))))
    css-parser::data))

(defun metapeg-action403 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action403
    "(-> `(,a ,b) `(id ,b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::id (list css-parser::b)))))
    css-parser::data))

(defun metapeg-action402 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action402
    "(-> `(,a) '(element *))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a) '(css-parser::element *))))
    css-parser::data))

(defun metapeg-action401 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action401
    "(-> `(,a) `(element ,a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a)
           (list* 'css-parser::element (list css-parser::a)))))
    css-parser::data))

(defun metapeg-action400 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action400
    "(-> `(,a) (if (cdr a) `(path ,@a) (first a)))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a)
           (if (cdr css-parser::a)
               (list* 'css-parser::path css-parser::a)
               (first css-parser::a)))))
    css-parser::data))

(defun metapeg-action399 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action399
    "(-> `(,a ,b) (if b `(path ,a ,@b) a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (if css-parser::b
               (list* 'css-parser::path (list* css-parser::a css-parser::b))
               css-parser::a))))
    css-parser::data))

(defun metapeg-action398 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action398
    "(-> `(,a) nil)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a) nil)))
    css-parser::data))

(defun metapeg-action397 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action397
    "(-> `(,a ,b) `(decendent ,b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::decendent (list css-parser::b)))))
    css-parser::data))

(defun metapeg-action396 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action396
    "(-> `(,a ,b ,c) `(,b ,c))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           (list* css-parser::b (list css-parser::c)))))
    css-parser::data))

(defun metapeg-action395 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action395
    "(->`(,a ,b) (if (or (not b) (eq b 'METAPEG::OPTIONAL)) a `(,(car b) ,a ,(second b))))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (if (or (not css-parser::b) (eq css-parser::b 'optional))
               css-parser::a
               (list* (car css-parser::b)
                      (list* css-parser::a (list (second css-parser::b))))))))
    css-parser::data))

(defun metapeg-action394 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action394
    "(-> `(,a ,b ,c) c)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           css-parser::c)))
    css-parser::data))

(defun metapeg-action393 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action393
    "(-> `(,a ,b  ,c) `(,b ,@c))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           (list* css-parser::b css-parser::c))))
    css-parser::data))

(defun metapeg-action392 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action392
    "(-> `(,a ,b ,c ,d ,e ,f) b)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a
                  (list* css-parser::b
                         (list* css-parser::c
                                (list* css-parser::d
                                       (list*
                                        css-parser::e
                                        (list css-parser::f))))))
           css-parser::b)))
    css-parser::data))

(defun metapeg-action391 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action391
    "(-> `(,a ,b ,c) c)"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           css-parser::c)))
    css-parser::data))

(defun metapeg-action390 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action390
    "(-> `(,a ,b) (if b `(or ,a ,@b) a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (if css-parser::b
               (list* 'or (list* css-parser::a css-parser::b))
               css-parser::a))))
    css-parser::data))

(defun metapeg-action389 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action389
    "(-> `(,a ,b) `(css-rule ,a -> ,@b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::css-rule
                  (list* css-parser::a
                         (list* 'css-parser::-> css-parser::b))))))
    css-parser::data))

(defun metapeg-action388 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action388
    "(-> `(,a) (make-number a))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list css-parser::a)
           (css-parser::make-number css-parser::a))))
    css-parser::data))

(defun metapeg-action387 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action387
    "(-> `(,a ,b ,c) (make-number `(,a #\\. ,c)))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::->
           (list* css-parser::a (list* css-parser::b (list css-parser::c)))
           (css-parser::make-number (list* css-parser::a
                                           (list*
                                            #\.
                                            (list css-parser::c)))))))
    css-parser::data))

(defun metapeg-action386 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action386
    " (-> `(,a ,b) (intern-ident `(,a ,b))) "
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (css-parser::intern-ident (list* css-parser::a
                                            (list css-parser::b))))))
    css-parser::data))

(defun metapeg-action385 (css-parser::data)
  (css-parser::trace-reducing
    'metapeg-action385
    "(-> `(,a ,b) `(stylesheet ,@b))"
    #'(lambda (css-parser::data)
        (macrolet ((css-parser::-> (css-parser::pattern &body css-parser::body)
                     (list* 'progn
                            (list* '(setf (cdr (last css-parser::data 2)) nil)
                                   (list (list*
                                          'fare-matcher:match
                                          (list*
                                           'css-parser::data
                                           (list
                                            (list*
                                             css-parser::pattern
                                             css-parser::body)))))))))
          (css-parser::-> (list* css-parser::a (list css-parser::b))
           (list* 'css-parser::stylesheet css-parser::b))))
    css-parser::data))