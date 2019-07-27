#lang racket

(require plot)

(define (pow5 x)
  (let ([x2 (* x x)])
    (* x2 x2 x)))

(define (pow6 x)
  (let ([x2 (* x x)])
    (* x2 x2 x2)))

(define (schlick-f0 n0 n1)
  (let ([t (/ (- n0 n1) (+ n0 n1))])
    (* t t)))

(define (schlick wo-dot-h f0)
  (+ f0
     (* (pow5 (- 1.0 wo-dot-h))
        (- 1.0 f0))))

(define (degrees->cos x)
  (- 1.0 (/ x 90.0)))

(define (lazanyi-schlick-a f0 f82)
  (let ([cos-theta-max (/ 1.0 7.0)])
    (/ (- (+ f0
             (- 1.0
                (* f0 (pow5 (- 1.0 cos-theta-max)))))
          f82)
       (* cos-theta-max
          (pow6 (- 1.0 cos-theta-max))))))

(define (lazanyi-schlick wo-dot-h f0 a)
  (- (schlick wo-dot-h f0)
     (* a wo-dot-h (pow6 (- 1.0 wo-dot-h)))))

(define ior 2.8)
;; (define f0 (schlick-f0 1.0 ior))
(define f0 0.8)
(define f82 1.0)

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
 #:width 800
 #:legend-anchor 'bottom-left
 "fresnel.png")

;; (plot-file 
;;  (function (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
;;            #:y-min 0.0
;;            #:label "y = schlick(x)")
;;  "test.png")
