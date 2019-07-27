#lang at-exp racket
(require infix)
(require plot)

(define (schlick-f0 n0 n1)
  (let ([t @${(n0 - n1) / (n0 + n1)}])
    @${t * t}))

(define (schlick costheta f0)
  @${f0 + (1.0 - costheta)^5 * (1.0 - f0)})

(define (degrees->cos x)
  @${1.0 - x / 90.0})

(define (lazanyi-schlick-a f0 f82)
  (let ([costhetamax (/ 1.0 7.0)])
    @${(f0 + (1.0 - f0) * (1.0 - costhetamax)^5 - f82)
       /
       (costhetamax * (1.0 - costhetamax)^6)}))

(define (lazanyi-schlick costheta f0 a)
 @${schlick[costheta, f0] - a * costheta * (1.0 - costheta)^6})

(define ior 2.8)
;; (define f0 (schlick-f0 1.0 ior))
(define f0 0.8)
(define f82 0.8)

(define a (lazanyi-schlick-a f0 f82))

f0
a

(plot-file
 (list (function (lambda (x) (schlick (degrees->cos x) f0)) 0 90
                 #:label "schlick"
                 #:color 0)
       (function (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
                 #:label "lazanyi-schlick"
                 #:color 1))
 #:y-min 0.0
 #:width 512
 #:height 512
 #:legend-anchor 'bottom-left
 "fresnel.png")

;; (plot-file 
;;  (function (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
;;            #:y-min 0.0
;;            #:label "y = schlick(x)")
;;  "test.png")
