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
        [label-0 (string-append "IoR " (number->string ior) "\t")]
        [label-n "\t\t"])
    (list (function
           (lambda (x) (schlick (degrees->cos x) f0)) 0 90
           #:label (string-append label-0 "Schlick")
           #:color color
           #:width 1.5)
          (function
           (lambda (x) (dielectric-reflect (degrees->cos x) 1.0 ior)) 0 90
           #:label (string-append label-n "Dielectric")
           #:color color
           #:width 1.5
           #:style 'dot)
          )))

(define plot-dimensions 720)

;; (plot-file
;;  (list (schlick-vs-dielectric 1.3 0)
;;        (schlick-vs-dielectric 1.6 1)
;;        (schlick-vs-dielectric 1.9 2))
;;  #:y-min 0.0
;;  #:width plot-dimensions
;;  #:height plot-dimensions
;;  #:x-label "Angle °"
;;  #:y-label "Reflection"
;;  #:legend-anchor 'top-left
;;  "schlick_dielectric.png")

(define (schlick-vs-conductor eta k color)
  (let* ([f0 (conductor 1.0 eta k)]
         [f82 (conductor (/ 1.0 7.0) eta k)]
         [a (lazanyi-schlick-a f0 f82)]
         [label-0 (string-append "η " (number->string eta) "\tκ " (number->string k) "\t")]
         [label-n "\t\t\t\t"])
    (list (function
           (lambda (x) (schlick (degrees->cos x) f0)) 0 90
           #:label (string-append label-0 "Schlick")
           #:color color
           #:width 1.5)
          (function
           (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
           #:label (string-append label-n "Lazányi-Schlick")
           #:color color
           #:width 1.5
           #:style 'long-dash)
          (function
           (lambda (x) (conductor (degrees->cos x) eta k)) 0 90
           #:label (string-append label-n "Conductor")
           #:color color
           #:width 1.5
           #:style 'dot)
          )))

;; (plot-file
;;  (list (schlick-vs-conductor 0.14 4.0 2)
;;        (schlick-vs-conductor 1.5 7.6 0)
;;        (schlick-vs-conductor 3.7 2.9 1))
;;  #:y-min 0.0
;;  #:width plot-dimensions
;;  #:height plot-dimensions
;;  #:x-label "Angle °"
;;  #:y-label "Reflection"
;;  #:legend-anchor 'bottom-left
;;  "schlick_conductor.png")


(define (rgb-fresnel reta geta beta rk gk bk)
  (define (channel eta k color)
    (let* ([f0 (conductor 1.0 eta k)]
           [f82 (conductor (/ 1.0 7.0) eta k)]
           [a (lazanyi-schlick-a f0 f82)])
      (list (function
             (lambda (x) (schlick (degrees->cos x) f0)) 0 90
             #:color color
             #:width 2.5)
            (function
             (lambda (x) (lazanyi-schlick (degrees->cos x) f0 a)) 0 90
             #:color color
             #:width 2.5
             #:style 'long-dash)
            (function
             (lambda (x) (conductor (degrees->cos x) eta k)) 0 90
             #:color color
             #:width 2.5
             #:style 'dot))))
  (list (channel reta rk 'red)
        (channel geta gk 'green)
        (channel beta bk 'blue)))

;; (plot-foreground 'white)
;; (plot-background 'black)
;; (plot-decorations? #f)

(define (plot-rgb-fresnel reta geta beta rk gk bk filename)
  (plot-file
   (rgb-fresnel reta geta beta rk gk bk) 
   #:y-min 0.0
   #:width 640
   #:height 640
   filename))

;; (plot-rgb-fresnel 0.13708 0.12945 0.14075 4.0625 3.1692 2.6034 "silver.png")

;; (plot-rgb-fresnel 3.11847 3.02492 2.44207 3.3190 3.3322 3.2034 "chromium.png")

;; (plot-rgb-fresnel 1.50694 0.926041 0.68251 7.6307 6.3849 5.6230 "aluminium.png")

(define etas (vector
              1.50694 0.926041 0.68251
              3.11847 3.02492 2.44207 
              2.86991 2.19091 1.80915    
              0.23268 1.075714 1.15753  
              0.18267 0.49447 1.3761    
              2.54731 2.13062 1.86627   
              0.22336 0.20973 0.23001   
              3.72055 3.70022 3.15441   
              1.99412 1.73204 1.64542   
              3.85401 4.95607 5.31627  
              1.78071 1.59124 1.44152  
              2.35423 2.06875 1.88085  
              2.17407 1.92204 1.81356   
              0.13708 0.12945 0.14075   
              1.65178 2.61195 2.83282   
              2.18385 1.8364  1.72103 
              3.66845 3.48243 3.31552   
              3.51454 3.66353 3.23655   
              2.9038  2.8857  2.6420  
              ))

(define ks (vector
            7.6307 6.3849 5.6230 
            3.3190 3.3322 3.2034 
            4.3419 3.8804 3.4920 
            3.5315 2.5945 2.4473 
            3.1178 2.3515 1.8324 
            4.6443 4.2061 3.7653 
            2.9727 2.3544 1.9843 
            3.5606 3.6863 3.5371
            3.7682 3.1316 2.7371 
            1.6997 1.8716 3.2281 
            4.3345 3.7268 3.3428 
            4.1988 3.6120 3.2271
            5.6790 4.8682 4.5063
            4.0625 3.1692 2.6034
            2.1475 1.8786 2.0726
            2.9550 2.5188 2.3034
            2.9274 2.7200 2.5721
            2.9684 3.0917 3.3679
            3.0798 2.9157 2.8025
            ))

(define f0s (vector-map (lambda (eta k) (conductor 1.0 eta k)) etas ks))

(define f82s (vector-map (lambda (eta k) (conductor (/ 1.0 7.0) eta k)) etas ks))

(define as (vector-map (lambda (f0 f82) (lazanyi-schlick-a f0 f82)) f0s f82s))

(plot-file
 (points (vector-map vector f0s as))
 #:x-min 0.0
 #:x-max 1.0
 #:y-min -1.0
 #:y-max 5.0
 #:x-label "f0"
 #:y-label "a"
 "points.png")
