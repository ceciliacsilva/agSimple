#lang racket

;; Caso o tamanho da populacao inicial seja impar, nas proximas sera size-1

;;USE:
;;> (find-min "simul3" *ag*)


(require plot)
(plot-new-window? #t)

(define-struct ag-parametros
  (size numeroMax cromossomoMax pc pm max maxRepeat))

(define *ag* (make-ag-parametros 50 512 1024 0.6 0.01 200 10))
(define *c* 430)

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

(define (find-min simulacaoNome ag)
  (make-directory* simulacaoNome)
  (let ( (popSize  (ag-parametros-size ag))
         (maxInteracoes (ag-parametros-max ag))
         (maxRepetido   (ag-parametros-maxRepeat ag)) )
    (let* ( (popInicial    (populacao-inicial ag))
            (roletaInicial (roleta-criar popInicial ag)) )
      (plot-populacao popInicial (string-append simulacaoNome "/geracao0.png") ag)
      (let loop ( (pop0   popInicial)
                  (roleta roletaInicial)
                  (best   '())
                  (repeatBest  0)
                  (repeat      1)
                  (bestList '()) )
        (let ( (pop1
                (for/fold ( (pop1 '()) )
                          ( (i (in-range (/ popSize 2))) )
                  (values (append pop1 (operacaoesGeneticas pop0 roleta ag))) ))  )
          (let ( (bestPop (first (sort pop1 #:key cdr >))) )
            (plot-populacao pop1 (string-append simulacaoNome "/geracao" (number->string repeat) ".png") ag)
            (if (or (= repeat maxInteracoes) (= repeatBest maxRepetido))
                (begin
                  (plot-best g best "g" ag)
                  (plot-best f (cons (car best) (- (cdr best))) "f" ag #t)
                  (script-video simulacaoNome)
                  (displayln (~a "x para f(x) mínimo: " (cromossomo->numero (car best) ag) " valor: " (cdr best)))
                  best)
                (loop pop1
                      (roleta-criar pop1 ag)
                      bestPop
                      (if (equal? best bestPop) (add1 repeatBest) 0)
                      (add1 repeat)
                      (cons bestPop bestList))  )
            )
          )
        ))
    ))

(define (plot-best funcao ponto namePlot ag [negativo? #f])
  (let ( (numero-max (ag-parametros-numeroMax ag)) )
    (plot
     (list
      (function funcao 0 numero-max #:label namePlot)
      (point-label (list (cromossomo->numero (car ponto) ag) (cdr ponto))) )
      #:x-min 0 #:x-max numero-max #:y-min 0 #:y-max (if negativo? (- *c*) *c*)
      )) )

(define (script-video simulacaoNome)
  (let ( (command "ffmpeg -framerate 4 -i geracao%000d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p output.mp4") )
    (call-with-output-file (string-append simulacaoNome "/toVideo.sh") #:exists 'replace
      (lambda(p)
        (displayln command p)) )) )

(define (plot-populacao pop fileOutput ag)
  (let ( (numero-max (ag-parametros-numeroMax ag)) )
    (plot-file
     (points (for/list ( (individuo (in-list pop)) )
               (list (cromossomo->numero (car individuo) ag) (cdr individuo)) )
             #:alpha 0.6
             #:sym 'fullcircle1
             #:color "blue"
             #:label fileOutput)
     #:x-min 0 #:x-max numero-max #:y-min 0 #:y-max *c*
   fileOutput )  ))

(define (populacao-inicial ag)
  (let ( (pop-size (ag-parametros-size ag))
         (numero-max (ag-parametros-numeroMax ag))
         (cromossomo-max (ag-parametros-cromossomoMax ag)) )
    (for/list ( (i (in-range pop-size)) )
      (let* ( (numero     (random cromossomo-max))
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


(define (operacaoesGeneticas pop0 roleta ag)
  (let* ( (pc (ag-parametros-pc ag))
          (pm (ag-parametros-pm ag))
          (cromossomoMax (ag-parametros-cromossomoMax ag))
          (cromossomoSize (exact-round (/ (log cromossomoMax) (log 2)))) )
    (let-values ( ((filho1 filho2) (crossover pop0 roleta cromossomoSize pc)) )
      (list (cons (mutacao filho1 cromossomoSize pm) (fitness-eval filho1 ag))
            (cons (mutacao filho2 cromossomoSize pm) (fitness-eval filho2 ag)))
      )
    ))

(define (crossover pop0 roleta cromossomoSize pc)
  (let* ( (individuo1 (list-ref pop0 (roleta-escolher roleta)))
          (individuo2 (list-ref pop0 (roleta-escolher roleta)))
          (pai (car individuo1))
          (mae (car individuo2)) )
    (let ( (cross? (random)) )
      (cond ( (> cross? pc) (values pai mae) )
            ( else
              (let* ( (posicao (random cromossomoSize))
                      (pai1    (substring pai 0 posicao))
                      (pai2    (substring pai posicao))
                      (mae1    (substring mae 0 posicao))
                      (mae2    (substring mae posicao)) )
                (let ( (filho1 (string-append pai1 mae2))
                       (filho2 (string-append mae1 pai2)) )
                  (values filho1 filho2) )  ))  )
      ) ) )

(define (mutacao cromossomo cromossomoSize pm)
  (let ( (mutacao? (random)) )
    (if (> mutacao? pm)
        cromossomo
        (let* ( (posicao (random cromossomoSize))
                (cromossomoList (string->list cromossomo))
                (geneMutacao    (list-ref cromossomoList posicao))
                (newCromossomoL (list-set cromossomoList posicao (notGene geneMutacao))) )
          (list->string newCromossomoL)
          )  )
    ))
                                             
(define (notGene gene)
  (if (equal? gene #\0) #\1 #\0))
      
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
    (let ( (r< (lambda(x) (< r x))) )
      ;;(displayln r)
      ;;(displayln roleta)
      (first (indexes-where roleta r<)) )) )

;;convert
(define (numero->cromossomo numero ag)
  (number->string numero 2))

(define (cromossomo->numero cromossomo ag)
  (normalize (string->number cromossomo 2) ag #f))

(define (normalize x ag [toCromossomo? #t])
  (let ( (cromossomoMax (ag-parametros-cromossomoMax ag))
         (numeroMax     (ag-parametros-numeroMax ag)) )
    (cond (toCromossomo? (* (/ cromossomoMax numeroMax) x))
          (else (* (/ numeroMax cromossomoMax) x)))  ))
