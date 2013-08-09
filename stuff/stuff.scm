(module stuff *
(import chicken scheme srfi-1 extras data-structures ports files foreign)

;; program    P ::= "(" "lambda" "(" id ")" e ")"
;; expression e ::= "0" | "1" | id
;;               | "(" "if0" e e e ")"
;;               | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
;;               | "(" op1 e ")"
;;               | "(" op2 e e ")"
;;          op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
;;          op2 ::= "and" | "or" | "xor" | "plus" 
;;          id  ::= [a-z]+

;; A valid program P contains at most one occurrence of "fold".

;; The expression "(fold e0 e1 (lambda (x y) e2))" uses the lambda-term
;; to fold over each byte of e0 bound to x (starting from the least
;; significant), and with the accumulator y initially bound to e1.

;; For example, given P = (lambda (x) (fold x 0 (lambda (y z) (or y z)))), 

;;    P(0x1122334455667788) 

;;    reduces to 

;;    (or 0x0000000000000011 
;;    (or 0x0000000000000022 
;;    (or 0x0000000000000033 
;;    (or 0x0000000000000044 
;;    (or 0x0000000000000055 
;;    (or 0x0000000000000066 
;;    (or 0x0000000000000077 
;;    (or 0x0000000000000088 
;;        0x0000000000000000))))))))

(define (deep-map p f tree)
 (cond ((p tree) (f tree))
       ((list? tree) (map (lambda (subtree) (deep-map p f subtree)) tree))
       (else tree)))

#>
#include <endian.h>
<#

(define wh#0 '#${0000000000000000})
(define wh#1 '#${0000000000000001})
(define wh#plus (foreign-primitive scheme-object
                            ((blob a)
                             (blob b))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = *((uint64_t*)a) + *((uint64_t*)b);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#and (foreign-primitive scheme-object
                           ((blob a)
                            (blob b))
                           "C_word *r = C_alloc(16);"
                           "uint64_t u = *((uint64_t*)a) & *((uint64_t*)b);"
                           "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#or  (foreign-primitive scheme-object
                           ((blob a)
                            (blob b))
                           "C_word *r = C_alloc(16);"
                           "uint64_t u = *((uint64_t*)a) | *((uint64_t*)b);"
                           "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#xor (foreign-primitive scheme-object
                           ((blob a)
                            (blob b))
                           "C_word *r = C_alloc(16);"
                           "uint64_t u = *((uint64_t*)a) ^ *((uint64_t*)b);"
                           "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#not (foreign-primitive scheme-object
                           ((blob a))
                           "C_word *r = C_alloc(16);"
                           "uint64_t u = ~*((uint64_t*)a);"
                           "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shlN (foreign-primitive scheme-object
                            ((blob a) (integer n))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) << n);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shl1 (foreign-primitive scheme-object
                            ((blob a))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) << 1);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shl8 (foreign-primitive scheme-object
                            ((blob a))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) << 8);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shrN (foreign-primitive scheme-object
                            ((blob a) (integer n))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) >> n);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shr1 (foreign-primitive scheme-object
                            ((blob a))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) >> 1);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shr4 (foreign-primitive scheme-object
                            ((blob a))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) >> 4);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shr8 (foreign-primitive scheme-object
                            ((blob a))
                            "C_word *r = C_alloc(16);"
                            "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) >> 8);"
                            "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#shr16 (foreign-primitive scheme-object
                             ((blob a))
                             "C_word *r = C_alloc(16);"
                             "uint64_t u = htobe64(be64toh(*((uint64_t*)a)) >> 16);"
                             "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define wh#random (foreign-primitive scheme-object ()
                                "C_word *r = C_alloc(16);"
                                "uint64_t u = (((uint64_t)rand()) << 32 | (uint64_t)rand());"
                                "C_return(C_bytevector(&r, 8, (char*)&u));"))
(define (wh#if0 c t e) (if (equal? c wh#0) t e))
(define (wh#fold blob i f)
 (let ((mask '#${00000000000000ff}))
  (let loop ((blob blob) (o 0) (r i))
   (if (= o 8)
       r
       (loop (wh#shr8 blob)
             (+ o 1)
             (f (wh#and mask blob) r))))))
)
