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

(declare (uses rpc secret more-stuff))
(use stuff scheme2c-compatibility nondeterminism traversal)
;;(solve-training-problem 5)



(define problems (vector->list (make-myproblems-call my-secret)))
(define possible-problems (filter (lambda (p)
				    (let
					((opl (vector->list (cdr (assoc 'operators p)))))
				      (and
				       ;; Stricly speaking also filter things we failed to solve
				       ;; probably with timeleft of 0
				       (not (assoc 'solved p))
				       (not (member "fold" opl))
				       (not (member "tfold" opl))
				       (< (cdr (assoc 'size p)) 8)
;;				       (eq? (cdr (assoc 'id p)) '0dO2InyjjBuzqwxqb2mvUhhX)
				       (< (vector-length (cdr (assoc 'operators p)))))))
				  problems))
(pretty-print possible-problems)
(display (length possible-problems))
(display "\n")
;; Try and just solve the head
(display (car possible-problems))
(define my-problem (car possible-problems))
(display (list "Tring to solve " (cdr (assoc 'id my-problem))))
(solve-problem (cdr (assoc 'id my-problem)) (cdr (assoc 'size my-problem)) (cdr (assoc 'operators my-problem)))
(display "\n")