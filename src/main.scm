;; TOOD: something
(declare (uses rpc secret))
;; For now lets just test if this can call something
(display "Using secret ")
(display my-secret)
(display "\n")
(display "call result was")
(make-train-call my-secret read-string)