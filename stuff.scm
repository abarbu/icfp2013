;; ;; myproblems
;; (pp (with-input-from-request
;;      "http://icfpc2013.cloudapp.net/myproblems?auth=04683EUSFXg7YCiEYqB0PzBEnGDUAAxpe8ZDxdruvpsH1H"
;;      #f read-json))

;; (pp (with-input-from-request
;;      "http://icfpc2013.cloudapp.net/train?auth=04683EUSFXg7YCiEYqB0PzBEnGDUAAxpe8ZDxdruvpsH1H"
;;      (json->string '((size . 27) (operators . #("tfold"))))
;;      read-json))

;; program    P ::= "(" "lambda" "(" id ")" e ")"
;; expression e ::= "0" | "1" | id
;;               | "(" "if0" e e e ")"
;;               | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
;;               | "(" op1 e ")"
;;               | "(" op2 e e ")"
;;          op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
;;          op2 ::= "and" | "or" | "xor" | "plus" 
;;          id  ::= [a-z]+

;; A valid program P contains at most one occurrence of "fold".

;; The expression "(fold e0 e1 (lambda (x y) e2))" uses the lambda-term
;; to fold over each byte of e0 bound to x (starting from the least
;; significant), and with the accumulator y initially bound to e1.

;; For example, given P = (lambda (x) (fold x 0 (lambda (y z) (or y z)))), 

;;    P(0x1122334455667788) 

;;    reduces to 

;;    (or 0x0000000000000011 
;;    (or 0x0000000000000022 
;;    (or 0x0000000000000033 
;;    (or 0x0000000000000044 
;;    (or 0x0000000000000055 
;;    (or 0x0000000000000066 
;;    (or 0x0000000000000077 
;;    (or 0x0000000000000088 
;;        0x0000000000000000))))))))

(define (read-expr expr)
 (deep-map symbol?
           (lambda (s)
            (cond ((member s '(and or shl1 shr1 shr4 shr16 plus fold xor if0 not))
                   (string->symbol (conc 'wh\: s)))
                  (else s)))
           (read-from-string expr)))

(define wh:and bitwise-and)
(define wh:or bitwise-ior)
(define wh:shl1 (lambda (a) (arithmetic-shift a 1)))
(define wh:shr1 (lambda (a) (arithmetic-shift a -1)))
(define wh:shr4 (lambda (a) (arithmetic-shift a -4)))
(define wh:shr16 (lambda (a) (arithmetic-shift a -16)))
(define wh:plus +)
(define wh:xor bitwise-xor)
(define (wh:if0 c t e) (if (zero? c) t e))
(define wh:not bitwise-not)

(read-expr "(lambda (x_68323) (fold x_68323 0 (lambda (x_68323 x_68324) (xor (if0 (not (shr4 (and (xor (shr4 (shr16 (shr1 (or (and (not (shr1 (not x_68324))) 1) 1)))) x_68323) x_68323))) x_68323 0) x_68323))))")

(read-expr "(lambda (x_13591) (shr1 (shl1 (not (if0 (and (shl1 1) (or 1 x_13591)) x_13591 0)))))")
