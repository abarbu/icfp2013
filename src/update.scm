(declare (uses stuff rpc secret more-stuff))
(use stuff scheme2c-compatibility nondeterminism traversal)

(write-object-to-file
 (sort (vector->list (make-myproblems-call my-secret)) < (lambda (a) (cdr (assoc 'size a))))
 "problems.scm")
