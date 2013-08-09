;; TOOD: something
(declare (uses rpc secret hobostuff))
(use medea)
;; For now lets just test if this can call something
(display "Using secret ")
(display my-secret)
(display "\n")
(display "call result was")
(display (make-train-call my-secret 30 '()))