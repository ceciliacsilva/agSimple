#lang racket


(require plot)
(plot-new-window? #t)

(define-struct ag-parametros
  (size numeroMax cromossomoMax pc pm max maxRepeat))

(define *ag* (make-ag-parametros 50 512 1024 0.6 0.01 100 10))

(define f
  (lambda(x)
    (- (abs (* x (sin (sqrt (abs x))) ) ))  ))

;;make positive
(define g
  (lambda(x)
    (- (f x))) )

(define (fitness-eval cromossomo ag)
  (let ( (x (cromossomo->numero cromossomo ag)) )
    (g x) ))

(define (populacao-inicial ag)
  (let ( (pop-size (ag-parametros-size ag))
         (numero-max (ag-parametros-numeroMax ag)) )
    (for/list ( (i (in-range pop-size)) )
      (let* ( (numero     (random numero-max))
              (aux        (numero->cromossomo numero ag))
              (cromossomo (cromossomo-fill aux ag))
              (fitness    (fitness-eval cromossomo ag)) )
        (cons cromossomo fitness)
        ))
    ))

(define (cromossomo-fill aux ag)
  (let* ( (cromossomoMax (ag-parametros-cromossomoMax ag))
          (cromossomoSize (/ (log cromossomoMax) (log 2))) )
    (let* ( (len    (string-length aux))
            (toFill (exact-round (- cromossomoSize len))) )
      (string-append (build-string toFill (lambda(i) #\0))
                     aux)  )) )

(define (roleta-criar pop0 ag)
  (let ( (fitness-total (apply + (map cdr pop0))) )
    (let-values ( ((soma roletaRev)
                   (for/fold ( (acc 0)
                               (roleta '()) )
                             ( (individuo (in-list pop0)) )
                     (let* ( (fitness (cdr individuo))
                             (fitness-relativo (+ acc (/ fitness fitness-total))) )
                       (values fitness-relativo (cons fitness-relativo roleta)) ))   )  )
      (reverse roletaRev)
      )
    ))

(define (roleta-escolher roleta)
  (let ( (r (random)) )
    (let ( (r< (lambda(x) (< x r))) )
      ;;(displayln r)
      ;;(displayln roleta)
      (last (indexes-where roleta r<)) )) )

;;convert
(define (numero->cromossomo numero ag)
  (number->string (normalize numero ag) 2))

(define (cromossomo->numero cromossomo ag)
  (normalize (string->number cromossomo 2) ag #f))

(define (normalize x ag [toCromossomo? #t])
  (let ( (cromossomoMax (ag-parametros-cromossomoMax ag))
         (numeroMax     (ag-parametros-numeroMax ag)) )
    (cond (toCromossomo? (* (/ cromossomoMax numeroMax) x))
          (else (* (/ numeroMax cromossomoMax) x)))  ))
