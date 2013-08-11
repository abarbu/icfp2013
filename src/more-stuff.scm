(declare (unit more-stuff))
(use stuff scheme2c-compatibility nondeterminism traversal random-bsd)

(define (a-boolean-rand)
 (call-with-current-continuation
  (lambda (c)
   (let ((old-fail fail)
         (b (zero? (random 2))))
    (set-fail! (lambda () (set-fail! old-fail) (if *fail?* (c (not b)) (fail))))
    b))))

(define-syntax either-rand
 (syntax-rules ()
  ((_) (fail))
  ((_ a) a)
  ((_ a b ...) (if (a-boolean-rand) a (either-rand b ...)))))

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
(define wh#most (cons 'wh#if0 (cons 'wh#fold (append wh#op1 wh#op2))))

(define (minimum-possible-size operators)
 (+ (* 2 (length (set-intersectione operators wh#op1)))
    (* 3 (length (set-intersectione operators wh#op2)))
    (* 4 (if (member 'if0 operators) 1 0))
    (* 5 (if (member 'fold operators) 1 0))))

;; shifts
;; not not
;; and 0
;; or 1
;; if0 0/1
;; constraint satisfaction for skeletons
;; reasonable bounds on # of ops per type
;;  attach # to interned symbols; saves about 10% of the search space
;;    but doesn't pay back bookkeeping

(define (an-expression-of-size size allowed-operators locals)
 ;; the minimal expression size is 1
 (when (< size 1) (fail))
 (if (= size 1)
     (a-member-of (cons 'wh#0 (cons 'wh#1 locals)))
     (either-rand
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
        `(wh#fold ,e0 wh#0 (lambda ,vars ,e2))))
      (begin
       (unless (member 'wh#if0 allowed-operators) (fail))
       (let* ((size-e0 (an-integer-between 1 (- size 3)))
              (e0 (an-expression-of-size size-e0 allowed-operators locals))
              (size-e1 (an-integer-between 1 (- (- size size-e0) 2)))
              (e1 (an-expression-of-size size-e1 allowed-operators locals))
              (size-e2 (- (- (- size size-e0) size-e1) 1))
              (e2 (an-expression-of-size size-e2 allowed-operators locals)))
        `(wh#if0 ,e0 ,e1 ,e2)))
      (let ((op1 (a-member-of (set-intersectione wh#op1 allowed-operators)))
            (body (an-expression-of-size (- size 1) allowed-operators locals)))
       `(,op1 ,body))
      ;; This is not valid because programs can't contain lambdas
      ;; (let* ((var (gensym 'b))
      ;;        (body (an-expression-of-size (- size 1) allowed-operators (cons var locals))))
      ;;  `(lambda (,var) ,body))
      (let*
        ((op2 (a-member-of (set-intersectione wh#op2 allowed-operators)))
         ;; -2 because we pay 1 for the op1, min 1 for body1
         (size0 (an-integer-between 1 (- size 2)))
         (body0 (an-expression-of-size size0 allowed-operators locals))
         (size1 (- (- size size0) 1))
         (body1 (an-expression-of-size size1 allowed-operators locals)))
       `(,op2 ,body0 ,body1)))))

 (define (a-program-of-size size allowed-operators)
  (a-program-of-exactly-size (an-integer-between 2 size) allowed-operators))

(define (a-program-of-exactly-size size allowed-operators)
 (let ((var (gensym 'x)))
  `(lambda (,var)
    ,(an-expression-of-size
      (- size 1)
      wh#most
      ;; (append (take wh#op1 2) (take wh#op2 2))
      ;; allowed-operators
      (list var)))))

(define (0&1bit-test-sequence)
 (let loop ((n 0) (l (list wh#1)))
  (if (= n 64)
      l
      (loop (+ n 1) (cons (wh#shl1 (car l)) l)))))

(define (test-sequence)
 (cons
  '#${BFFFFEFFFFFFFFFF}
  (cons
   '#${FFFFFFFFFFFFFFFE}
   (cons
    '#${FFFFFFFFFFFFFFFF}
    (cons
     '#${FFFFFFFFFFFFFFFF}
     (cons
      '#${7FFFFFFFFFFFFFFF}
      (append
       (0&1bit-test-sequence)
       (map-n (lambda _ (wh#random)) (- 256 70)))))))))

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

;; (define (solve-training-problem size)
;;  (let* ((example (values (make-train-call1 my-secret size)))
;;         (inputs (test-sequence))
;;         (seq (values (make-eval-program-call my-secret (cdr (assoc 'challenge example)) inputs ))))
;;   (display (list example inputs seq))(newline)
;;   (make-guess-call my-secret
;;                    (cdr (assoc 'id example))
;;                    (format #f
;;                            "~a"
;;                            (unread-expr
;;                             (one-value
;;                              (let* ((code (a-program-of-size
;;                                            (cdr (assoc 'size example))
;;                                            (map (lambda (a) (string->symbol (conc 'wh# a))) (vector->list (cdr (assoc 'operators example))))))
;;                                     (f (eval code)))
;;                               (unless (every (lambda (in out) (equal? (f in) out)) inputs (map wh#read-from-string (vector->list (cdr (assoc 'outputs seq))))) (fail))
;;                               code)))))))

;; ((status . "mismatch") (values . #("0xBFFFFEFFFFFFFFFF" "0x0000BFFFFF000000" "0x0000BFFFFEFFFFFF")))
;; ((status . "win") (lightning . #t))

(define (solve-problem1-of-depth-with-data size data)
 (let ((id (first data))
       (operators (second data))
       (inputs (third data))
       (outputs (fourth data)))
  (one-value
   (let* ((code (a-program-of-exactly-size
                 size
                 (map (lambda (a) (string->symbol (conc 'wh# a))) (vector->list operators))))
          (f (eval code)))
    (unless (every (lambda (in out) (equal? (f in) out)) inputs outputs)
     (fail))
    code))))

(define (simple-solve-problem id size operators)
 (let* ((inputs (test-sequence))
        (seq (values (make-eval-id-call my-secret id inputs ))))
  (display (list id inputs seq size operators))(newline)
  (display (list id size operators))(newline)
  (display "Making guess call")(newline)
  (make-guess-call
   my-secret
   id
   (format #f
           "~a"
           (unread-expr
            (one-value
             (let* ((code (a-program-of-size
                           size
                           (map (lambda (a) (string->symbol (conc 'wh# a))) (vector->list operators))))
                    (f (eval code)))
              (unless (every (lambda (in out) (equal? (f in) out)) inputs (map wh#read-from-string (vector->list (cdr (assoc 'outputs seq))))) (fail))
              code)))))))

(define (solve-problem id size operators)
 (let* ((inputs (test-sequence))
        (seq (values (make-eval-id-call my-secret id inputs)))
        (outputs (map wh#read-from-string (vector->list (cdr (assoc 'outputs seq))))))
  (let loop ((prev-inputs inputs) (prev-outputs outputs) (n 0))
   (display (list id inputs seq))(newline)
   (display "Solving")(newline)
   (let* ((solution (time (format #f
                                  "~a"
                                  (unread-expr
                                   (one-value
                                    (let* ((code (a-program-of-size
                                                  size
                                                  (map (lambda (a) (string->symbol (conc 'wh# a))) (vector->list operators))))
                                           (f (eval code)))
                                     (unless (every (lambda (in out) (equal? (f in) out)) inputs outputs) (fail))
                                     code))))))
          (r (begin (display solution)(newline) (make-guess-call my-secret id solution))))
    (display r)(newline)
    (when (equal? (cdr (assoc 'status r)) "mismatch")
     (when (< n 3)
      (let ((a (map wh#read-from-string (cdr (assoc 'values r)))))
       (display "Trying again")(newline)
       (loop (cons (first a) prev-inputs) (cons (second a) prev-outputs) (+ n 1)))))))))

(define (solve-problem1 l)
 (solve-problem (cdr (assoc 'id l)) (cdr (assoc 'size l))(cdr (assoc 'operators l))))
