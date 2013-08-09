(declare (unit rpc))
(use medea http-client)
(client-software (list (list"Wandering Hobos" 0.1 #f)))
(define (make-train-call auth-code call-back)
  (with-input-from-request (make-req-url "train" auth-code) '((fake . "value")) call-back)
)
(define (make-req-url method auth-code)
  (string-append "http://icfpc2013.cloudapp.net/" method "?auth=" auth-code)
)
