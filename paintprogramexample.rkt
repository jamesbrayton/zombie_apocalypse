#lang racket/gui

;(require picturing-programs)
(require (planet williams/science/random-distributions))
(require (planet williams/science/math))
(require (planet williams/simulation/simulation)
         (planet williams/inference/inference))
(require (planet williams/animated-canvas/animated-canvas))

;; __________________________GLOBALS__________________________________

(define edge-length 10)
(struct wall-coords (i j w h))
  

;MOUSE CONTROL
(define (mouse-handle i j mouse-event% wall-coords)
  (cond [(string=? mouse-event% "button-up")
         (wall-coords wall-coords-i wall-coords-j (+ wall-coords-i i) (+ wall-coords-j j))]
 
        [(string=? mouse-event% "button-down")
         (wall-coords i j wall-coords-i wall-coords-j)]
        
        ;[(string=? mouse-event% "drag")
        ;(wall-coords wall-coords-i wall-coords-j (+ wall-coords-i i) (+ wall-coords-j j))]
        
        ))

;SIMPLE WALL CREATION IN CURRENT ZOMBIE GAME
(define get-wall (wall-coords 200 50 20 50))

(define (draw-walls dc)
  (send dc set-brush "black" 'solid)        
      (send dc draw-rectangle (wall-coords-i get-wall) (wall-coords-j get-wall) (wall-coords-w get-wall) (wall-coords-h get-wall)))


;; All the GFX
;;_____________________________________________________________
(define frame
  (instantiate frame% ("Zombie Apocalypse")))

(define canvas
  (instantiate animated-canvas%
    (frame)
    (style `(border))
    (min-width 400)
    (min-height 600)))

;;______________________________________________________________
  
;_____________________________RUN IT____________________________

(define (run-zombie-sim)
  (with-new-inference-environment
   ;; needed for top to bottom ordering of the rules
   (current-inference-strategy 'order)
   ;;(activate zombie-game-rules)
   ;; timer keeps track of the turns
   (assert '(time 1))                        
   ;; for an end state to see winner
   ;;(assert `(player-count ,players))                
   ;; create the players based on players GLOBAL
   ;;(for ((i (in-range players)))
   ;;  (assert `(start ,i)))
   (start-inference)))

;; randomize source
(random-source-randomize! (current-random-source))
;; run it
(send frame show #t)
(run-zombie-sim)