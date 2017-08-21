#lang racket

(require racket/gui plot racket/draw)

(define num 0)

(define f (new frame% [label "Test graph"]
               [width 200]
               [height 200]))
(define c (new canvas% [parent f]))


(send f show #t)

(define (loop)
  (set! num (add1 num))
  (plot/dc (function sin (- pi) num)
           (send c get-dc)
           0 0
           (- (send f get-width) 40) ;; figure out how to get the actual size of the text outside the graphs boarder?
           (- (send f get-height) 40)
           #:title "Graph"
           #:x-label "num"
           #:y-label "sin"
           )
  (sleep/yield .2)
  (loop))

(loop)