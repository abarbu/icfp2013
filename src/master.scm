(declare (uses stuff rpc secret more-stuff))
(use stuff scheme2c-compatibility nondeterminism traversal)

(let ((id (car (command-line-arguments))))
 (let ((problem (find (lambda (a) (equal? (cdr (assoc 'id a)) id))
                      (read-object-from-file "problems.scm"))))
  (when (and (assoc 'solved problem)
           (cdr (assoc 'solved problem)))
   (display "Already solved")(newline)
   (exit 0))
  (when (and (assoc 'timeLeft problem)
           (equal? (cdr (assoc 'timeLeft problem)) 0))
   (display "Already failed")(newline)
   (exit 0))
  (let* ((inputs (test-sequence))
         (seq (values (make-eval-id-call my-secret id inputs)))
         (outputs (map wh#read-from-string (vector->list (cdr (assoc 'outputs seq))))))
   (display "Writing data")(newline)
   (write-object-to-file (list id (cdr (assoc 'operators problem)) inputs outputs) (format #f "/tmp/data-~a.sc" id))
   (display "Starting slaves")(newline)
   (for-each-n
     (lambda (n)
      (system (format #f "./slave /tmp/data-~a.sc ~a /tmp/out-~a-~a.sc 0 -:hi2000m&"
                      id n id n))
      (system (format #f "./slave /tmp/data-~a.sc ~a /tmp/out-~a-~a.sc 1 -:hi2000m&"
                      id n id n)))
    30)
   (let ((depth&solution
          (call/cc (lambda (k)
                    (for-each-n
                      (lambda (s)
                       (for-each-n
                         (lambda (n)
                          (when (file-exists? (format #f "/tmp/out-~a-~a.sc" id n))
                           (format #t "Solved in ~a seconds~%" s)
                           (usleep 1000000) ;1s
                           (k (list n (read-object-from-file (format #f "/tmp/out-~a-~a.sc" id n))))))
                        30)
                       (usleep 1000000)) ;1s
                     300)
                    (display "Ran out of time")(newline)
                    (exit 0)))))
    (pp depth&solution)
    (system "killall slave")
    (display "Guess call")(newline)
    (let ((r (make-guess-call my-secret
                              id
                              (format #f "~a" (unread-expr (second depth&solution))))))
     (pp r)(newline)
     (exit (if (equal? (cdr (assoc 'status r)) "win") 0 1)))))))
