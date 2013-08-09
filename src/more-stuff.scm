(declare (unit more-stuff))
(use stuff scheme2c-compatibility nondeterminism traversal)

(define (read-expr expr)
 (deep-map (lambda (a) (or (symbol? a) (number? a)))
           (lambda (s)
            (cond ((member s '(and or shl1 shr1 shr4 shr16 plus fold xor if0 not 0 1))
                   (string->symbol (conc 'wh# s)))
                  (else s)))
           (read-from-string expr)))

(define (unread-expr expr)
 (deep-map (lambda (a) (or (symbol? a) (number? a)))
           (lambda (s)
            (cond ((member s '(wh#and wh#or wh#shl1 wh#shr1 wh#shr4 wh#shr16 wh#plus wh#fold wh#xor wh#if0 wh#not wh#0 wh#1))
                   (string->symbol (list->string (drop (string->list (symbol->string s)) 3))))
                  (else s)))
           expr))

(define (expr-size expr)
 (cond ((member expr '(wh#0 wh#1)) 1)
       ((list? expr)
        (case (first expr)
         ((wh#if0) (+ 1
                      (expr-size (second expr))
                      (expr-size (third expr))
                      (expr-size (fourth expr))))
         ((wh#fold) (+ 2
                       (expr-size (second expr))
                       (expr-size (third expr))
                       (expr-size (third (fourth expr)))))
         ((wh#not wh#shl1 wh#shr1 wh#shr4 wh#shr16)
          (+ 1 (expr-size (second expr))))
         ((wh#and wh#or wh#xor wh#plus)
          (+ 1
             (expr-size (second expr))
             (expr-size (third expr))))
         ((lambda) (+ 1 (expr-size (third expr))))
         (else (error "Bad expr list" expr))))
       (else (unless (symbol? expr) (error "Bad expr" expr))
             1)))

(define (expr-operators expr)
 (cond ((member expr '(wh#0 wh#1)) '())
       ((list? expr)
        (case (first expr)
         ((wh#if0)
          (remove-duplicatese
           (append (list 'wh#if0)
                   (expr-operators (second expr))
                   (expr-operators (third expr))
                   (expr-operators (fourth expr)))))
         ((wh#fold)
          (remove-duplicatese
           (append (list
                    (if (equal? (third expr) 'wh#0)
                        'wh#tfold
                        'wh#fold))
                   (expr-operators (second expr))
                   (expr-operators (third expr))
                   (expr-operators (third (fourth expr))))))
         ((wh#not wh#shl1 wh#shr1 wh#shr4 wh#shr16)
          (remove-duplicatese
           (append (list (first expr))
                   (expr-operators (second expr)))))
         ((wh#and wh#or wh#xor wh#plus)
          (remove-duplicatese
           (append (list (first expr))
                   (expr-operators (second expr))
                   (expr-operators (third expr)))))
         ;; lambda doesn't count
         ((lambda) (expr-operators (third expr)))
         (else (error "Bad expr list" expr))))
       (else (unless (symbol? expr) (error "Bad expr" expr))
             '())))

;; List of signal
(define wh#op1 '(wh#not wh#shl1 wh#shr1 wh#shr4 wh#shr16))
;; List of two
(define wh#op2 '(wh#and wh#or wh#xor wh#plus))

(define used-operators '())

(define (an-expression-of-size size allowed-operators locals)
 ;; the minimal expression size is 1
 (when (< size 1) (fail))
 (either
  (if (= size 1)
      (a-member-of (cons 'wh#0 (cons 'wh#1 locals)))
      (let* ((unused-operators (set-differencee allowed-operators used-operators))
	     (unused-wh#op1 (set-intersectione unused-operators wh#op1))
	     (unused-wh#op2 (set-intersectione unused-operators wh#op2))
	     (minsize (+ (* 2 (length unused-wh#op1)) (* 3 (length unused-wh#op2)) )))
	(if (> 0 size)
;;	    (pretty-print (list used-operators minsize allowed-operators))
  	    (fail)
	    (either
	     (let ((op1 (a-member-of (set-intersectione wh#op1 allowed-operators)))
		   (body (an-expression-of-size (- size 1) allowed-operators locals)))
	       (if (not (member (quote op1) used-operators))
	           (local-set! used-operators (cons (quote op1) used-operators))
	           )
	       `(,op1 ,body))
	     ;; This is not valid because programs can't contain lambdas
	     ;; (let* ((var (gensym 'b))
	     ;;        (body (an-expression-of-size (- size 1) allowed-operators (cons var locals))))
	     ;;  `(lambda (,var) ,body))
	     (either
	      (let*
		  ((op2 (a-member-of (set-intersectione wh#op2 allowed-operators)))
		   ;; -2 because we pay 1 for the op1, min 1 for body1
		   (size0 (an-integer-between 1 (- size 2)))
		   (body0 (an-expression-of-size size0 allowed-operators locals))
		   (size1 (- (- size size0) 1))
		   (body1 (an-expression-of-size size1 allowed-operators locals)))
		(if (not (member (quote op2) used-operators))
		    (local-set! used-operators (cons (quote op2) used-operators))
		    )
		`(,op2 ,body0 ,body1))
	      (either
	       (begin
		 (unless (member 'wh#if0 allowed-operators) (fail))
		 (let* ((size-e0 (an-integer-between 1 (- size 3)))
			(e0 (an-expression-of-size size-e0 allowed-operators locals))
			(size-e1 (an-integer-between 1 (- (- size size-e0) 2)))
			(e1 (an-expression-of-size size-e1 allowed-operators locals))
			(size-e2 (- (- (- size size-e0) size-e1) 1))
			(e2 (an-expression-of-size size-e2 allowed-operators locals)))
		   `(wh#if0 ,e0 ,e1 ,e2)))
	       (either
		(begin
		  (unless (member 'wh#fold allowed-operators) (fail))
		  (let* ((vars (list (gensym 'x) (gensym 'y)))
			 (new-allowed-operators (removee 'wh#fold allowed-operators))
			 (size-e0 (an-integer-between 1 (- size 4)))
			 (e0 (an-expression-of-size size-e0 new-allowed-operators locals))
			 (size-e1 (an-integer-between 1 (- (- size size-e0) 3)))
			 (e1 (an-expression-of-size size-e1 new-allowed-operators locals))
			 (size-e2 (- (- (- size size-e0) size-e1) 2))
			 (e2 (an-expression-of-size size-e2 new-allowed-operators
						    (append vars locals))))
		    `(wh#fold ,e0 ,e1 (lambda ,vars ,e2))))
		(begin
		  (unless (member 'wh#tfold allowed-operators) (fail))
		  (let* ((vars (list (gensym 'x) (gensym 'y)))
			 (new-allowed-operators (removee 'wh#tfold allowed-operators))
			 (e0 (a-member-of locals))
			 (size-e2 (- size 4))
			 (e2 (an-expression-of-size size-e2 new-allowed-operators (append vars locals))))
		    `(wh#fold ,e0 wh#0 (lambda ,vars ,e2)))))))))))))

(define (a-program-of-size size allowed-operators)
  (let ((var (gensym 'x)))
    `(lambda (,var)
       ,(an-expression-of-size
	 (- size 1)
	 allowed-operators
	 (list var)))))

(define (0&1bit-test-sequence)
 (let loop ((n 0) (l (list wh#1)))
  (if (= n 64)
      l
      (loop (+ n 1) (cons (wh#shl1 (car l)) l)))))

(define (test-sequence)
 (append
  (0&1bit-test-sequence)
  (map-n (lambda _ (wh#random)) (- 256 65))))

(define (test-everything key)
 (let* ((example (values (make-train-call1 key 5)))
        (seq (values (make-eval-program-call key (assoc 'challenge example) (test-sequence)))))
  
  (list example seq)))

(define (wh#read-from-string string)
 (read-from-string (list->string (append '(#\# #\$ #\{) (drop (string->list string) 2) '(#\})))))

;; (read-expr "(lambda (x_68323) (fold x_68323 0 (lambda (x_68323 x_68324) (xor (if0 (not (shr4 (and (xor (shr4 (shr16 (shr1 (or (and (not (shr1 (not x_68324))) 1) 1)))) x_68323) x_68323))) x_68323 0) x_68323))))")

;; (read-expr "(lambda (x_13591) (shr1 (shl1 (not (if0 (and (shl1 1) (or 1 x_13591)) x_13591 0)))))")

;; ((eval (read-expr "(lambda (x_13591) (shr1 (shl1 (not (if0 (and (shl1 1) (or 1 x_13591)) x_13591 0)))))")) wh#1)

;; (all-values (an-expression-of-size 3 '(wh#and wh#not) '(x y z)))

(define (solve-training-problem size)
 (let* ((example (values (make-train-call1 my-secret size)))
        (inputs (test-sequence))
        (seq (values (make-eval-program-call my-secret (cdr (assoc 'challenge example)) inputs ))))
  (display (list example inputs seq))(newline)
  (make-guess-call my-secret
                   (cdr (assoc 'id example))
                   (format #f
                           "~a"
                           (unread-expr
                            (one-value
                             (let* ((code (a-program-of-size
                                           (cdr (assoc 'size example))
                                           (map (lambda (a) (string->symbol (conc 'wh# a))) (vector->list (cdr (assoc 'operators example))))))
                                    (f (eval code)))
                              (unless (every (lambda (in out) (equal? (f in) out)) inputs (map wh#read-from-string (vector->list (cdr (assoc 'outputs seq))))) (fail))
                              code)))))))
