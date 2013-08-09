(use stuff)

;; myproblems
(pp (with-input-from-request
     "http://icfpc2013.cloudapp.net/myproblems?auth=04683EUSFXg7YCiEYqB0PzBEnGDUAAxpe8ZDxdruvpsH1H"
     #f read-json))

(pp (with-input-from-request
     "http://icfpc2013.cloudapp.net/train?auth=04683EUSFXg7YCiEYqB0PzBEnGDUAAxpe8ZDxdruvpsH1H"
     (json->string '((size . 27) (operators . #("tfold"))))
     read-json))

(define (read-expr expr)
 (deep-map (lambda (a) (or (symbol? a) (number? a)))
           (lambda (s)
            (cond ((member s '(and or shl1 shr1 shr4 shr16 plus fold xor if0 not 0 1))
                   (string->symbol (conc 'wh\: s)))
                  (else s)))
           (read-from-string expr)))

(read-expr "(lambda (x_68323) (fold x_68323 0 (lambda (x_68323 x_68324) (xor (if0 (not (shr4 (and (xor (shr4 (shr16 (shr1 (or (and (not (shr1 (not x_68324))) 1) 1)))) x_68323) x_68323))) x_68323 0) x_68323))))")

(read-expr "(lambda (x_13591) (shr1 (shl1 (not (if0 (and (shl1 1) (or 1 x_13591)) x_13591 0)))))")

((eval (read-expr "(lambda (x_13591) (shr1 (shl1 (not (if0 (and (shl1 1) (or 1 x_13591)) x_13591 0)))))")) wh:1)
