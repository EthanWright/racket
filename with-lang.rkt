#lang plai

;; WAE data type

(define-type WAE
  [num (n number?)]
  [var (x symbol?)]
  [add (lhs WAE?) (rhs WAE?)]
  [mult (lhs WAE?) (rhs WAE?)]
  [with (x symbol?) (bind WAE?) (body WAE?)])

;; parse: sexp -> WAE

(define (parse sexp)
  (cond 
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (var sexp)]
    [(list? sexp) 
     (case (first sexp)
       [(+) (add (parse (second sexp)) (parse (third sexp)))]
       [(*) (mult (parse (second sexp)) (parse (third sexp)))]
       [(with) 
        (with (first (second sexp)) (parse (second (second sexp))) (parse (third sexp)))]
       [else (error "Syntax error")])]))
    
;; calc : WAE -> number

(define (calc e)
  (type-case WAE e
             [num (n) n]
             [add (y z) (+ (calc y) (calc z))]
             [mult (y z) (* (calc y) (calc z))]
             [with (x bind body) 
                   (calc (mysub x (num (calc bind)) body))]
             [var (x) (error "Free variable")]))

;; mysub: symbol * WAE * WAE -> WAE 

(define (mysub x bind body)
  (type-case WAE body
             [num (n) body]
             [var (y) (if (eq? x y) 
                          bind
                          body)]
             [add (e1 e2) (add (mysub x bind e1) (mysub x bind e2))]
             [mult (e1 e2) (mult (mysub x bind e1) (mysub x bind e2))]
             [with (y bind1 body1) 
                   (let* ([e1 (mysub x bind bind1)]
                          [e2 (mysub y e1 body1)])
                          (mysub x bind e2))]))


;; Test cases

;; (parse (read))
;; (parse '{+ 1 2})
;; (calc (parse '{+ 1 2}))

(calc (parse '{with {x 4} {with {y {* x x}} {+ x y}}}))

(calc (parse '{with {x 4} {with {x {* x x}} {+ x x}}}))