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
;;(solve-training-problem 5)



(define problems (vector->list (make-myproblems-call my-secret)))
(define possible-problems (filter (lambda (p)
				    (let
					((opl (vector->list (cdr (assoc 'operators p)))))
				      (and
				       ;; Stricly speaking also filter things we failed to solve
				       ;; probably with timeleft of 0
				       (or (not (assoc 'solved p))
					   (not (cdr (assoc 'solved p))))
				       (not (member "fold" opl))
				       (or
					(not (assoc 'timeLeft p))
					(not (equal? (cdr (assoc 'timeLeft p)) 0))
				       )
				       (not (member "tfold" opl))
				       (< (cdr (assoc 'size p)) 8)
				       (< (vector-length (cdr (assoc 'operators p)))))))
				  problems))
(pretty-print possible-problems)
(display (length possible-problems))
(display "\n")
;; Try and just solve the head
(sleep 20)
(map (lambda (my-problem)
       (display (list "Tring to solve" my-problem))
       (let ((problem-solution (solve-problem (cdr (assoc 'id my-problem)) (cdr (assoc 'size my-problem)) (cdr (assoc 'operators my-problem)))))
	 (display problem-solution)
	 (if (not (equal? (cdr (assoc 'status problem-solution)) "win"))
	     (error (list "We died on problem" my-problem))
	     )
	 )
       (display "\n")
       (display "Sleeping\n")
       (sleep 6)
       ) possible-problems)
