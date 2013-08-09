(when #f
 ;; TOOD: something
 (declare (uses rpc secret))
 (use medea)
 ;; For now lets just test if this can call something
 (display "Using secret ")
 (display my-secret)
 (display "\n")
 (display "call result was")
 (define train_results (make-train-call my-secret 3 '()))
 (display train_results)
 (display "\n")
 (display "Eval of above\n")
 (display (make-eval-program-call my-secret (cdr (assoc 'challenge train_results)) (list "0x00000000000001" "0xEFFFFFFFFFFFFF")))
 (display "Eval of shift left\n")
 (display (make-eval-program-call my-secret "(lambda (x) (shl1 x))" (list "0x00000000000001" "0xEFFFFFFFFFFFFF")))
 (display "Eval of shift right\n")
 (display (make-eval-program-call my-secret "(lambda (x) (shr1 x))" (list "0x00000000000001" "0xEFFFFFFFFFFFFF")))
 (display "Status\n")
 (display (make-status-call my-secret)))

(declare (uses stuff rpc secret more-stuff))
(use stuff scheme2c-compatibility nondeterminism traversal)
(solve-training-problem 8)
