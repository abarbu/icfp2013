(declare (unit rpc))
(use medea http-client)
(client-software (list (list "Wandering Hobos" 0.1 #f)))
(define (wh:format blob)
 (let ((str (format #f "~a" blob)))
  (string-append "0x" (list->string (drop (but-last (string->list str)) 3)))))
(define (make-train-call auth-code num operators)
  (make-call auth-code "train" 
	     (list (cons 'size  num)
		   (cons 'operators (list->vector operators))
		   )
	     )
  )
(define (make-train-call1 auth-code num)
  (make-call auth-code "train" 
	     (list (cons 'size  num))
	     )
  )
(define (make-myproblems-call auth-code)
  (make-call auth-code "myproblems" #f)
)
(define (make-eval-id-call auth-code id arguments) 
  (make-call auth-code "eval" (list
			       (cons 'id id)
			       (cons 'arguments (list->vector (map wh:format arguments)))))
  )
(define (make-eval-program-call auth-code program arguments)
  (make-call auth-code "eval" (list
			       (cons 'program program)
			       (cons 'arguments (list->vector (map wh:format arguments)))
			       ))
  )
(define (make-guess-call auth-code id program)
  (make-call auth-code "guess" (list
			       (cons 'program program)
			       (cons 'id id)))
  )
(define (make-status-call auth-code)
  (make-call auth-code "status" #f)
)
(define (make-call auth-code type params)
  (display "Making call ")
  (display params)
  (display "\n")
  (with-input-from-request (make-req-url type auth-code) (json->string params) read-json)
)
(define (make-req-url method auth-code)
  (string-append "http://icfpc2013.cloudapp.net/" method "?auth=" auth-code)
)
