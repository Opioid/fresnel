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

(define (dielectric costhetai costhetat etai etat)
  (let* ([t0 @${etat * costhetai}]
         [t1 @${etai * costhetat}]
         [rp @${(t0 - t1) / (t0 + t1)}]
         [t2 @${etai * costhetai}]
         [t3 @${etat * costhetat}]
         [ro @${(t2 - t3) / (t2 + t3)}])
    @${0.5 * (rp * rp + ro * ro)}))

(define (dielectric-reflect costhetai etai etat)
  (let* ([sinthetai @${sqrt[max[0.0, 1.0 - costhetai^2]]}]
         [sinthetat @${etai / etat * sinthetai}]
         [sint2 @${sinthetat^2}]
         [costhetat @${sqrt[1.0 - sint2]}])
    (dielectric costhetai costhetat etai etat)))

(define (conductor costhetai eta k)
  (let* ([tmpf @${eta^2 + k^2}]
         [costhetai2 @${costhetai^2}]
         [tmp @${costhetai2 * tmpf}]
         [a @${2.0 * costhetai * eta}]
         [rp @${(tmp - a + 1.0) / (tmp + a + 1.0)}]
         [ro @${(tmpf - a + costhetai2) / (tmpf + a + costhetai2)}])
    @${0.5 * (rp + ro)}))

(define (schlick-vs-dielectric ior color)
  (let ([f0 (schlick-f0 1.0 ior)]
        [label (string-append "IoR " (number->string ior) "\t")])
    (list (function
           (lambda (x) (schlick (degrees->cos x) f0)) 0 90
           #:label (string-append label "Schlick")
           #:color color)
          (function
           (lambda (x) (dielectric-reflect (degrees->cos x) 1.0 ior)) 0 90
           #:label (string-append label "Dielectric")
           #:color color
           #:style 'dot)
          )))

(define plot-dimensions 720)

(plot-file
 (list (schlick-vs-dielectric 1.3 0)
       (schlick-vs-dielectric 1.6 1)
       (schlick-vs-dielectric 1.9 2))
 #:y-min 0.0
 #:width plot-dimensions
 #:height plot-dimensions
 #:x-label "angle"
 #:y-label "reflection"
 #:legend-anchor 'top-left
 "schlick_dielectric.png")

(define (schlick-vs-conductor eta k color)
  (let* ([f0 (conductor 1.0 eta k)]
         [f82 (conductor (/ 1.0 7.0) eta k)]
         [a (lazanyi-schlick-a f0 f82)]
         [label (string-append "IoR " (number->string eta) "\tk " (number->string k) "\t")])
    (list (function
           (lambda (x) (schlick (degrees->cos x) f0)) 0 90
           #:label (string-append label "Schlick")
           #:color color)
          (function
           (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
           #:label (string-append label "Lazányi-Schlick")
           #:color color
           #:style 'long-dash)
          (function
           (lambda (x) (conductor (degrees->cos x) eta k)) 0 90
           #:label (string-append label "Conductor")
           #:color color
           #:style 'dot)
          )))

(plot-file
 (list (schlick-vs-conductor 0.14 4.0 2)
       (schlick-vs-conductor 1.5 7.6 0)
       (schlick-vs-conductor 3.7 2.9 1)
       )
 #:y-min 0.0
 #:width plot-dimensions
 #:height plot-dimensions
 #:x-label "angle"
 #:y-label "reflection"
 #:legend-anchor 'bottom-left
 "schlick_conductor.png")

;; (plot-file 
;;  (function (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
;;            #:y-min 0.0
;;            #:label "y = schlick(x)")
;;  "test.png")
