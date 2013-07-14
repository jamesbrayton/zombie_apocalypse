#lang racket/gui

(require (planet williams/science/random-distributions))
(require (planet williams/science/math))
(require (planet williams/simulation/simulation)
         (planet williams/inference/inference))

;; GLOBALS
(define edge-length 4)

;;______________________________________________________________

;; struct to hold people/zombies
(define-struct actor (i j type strength speed IQ)) 

;; calculates distance between actors
(define (distancer a1 a2)
  (sqrt (+ (expt (- (actor-i a1) (actor-i a2)) 2)
                (expt (- (actor-j a1) (actor-j a2)) 2))))




;; walk that just is [-1,1], apply to i and j
(define (rand-move)
  (+ (random-integer 3) -1))

;; pick a random spot within the bounds to start
(define (random-start)
  (random-integer edge-length))

;; within the bounds? uses edge-length GLOBAL
(define (valid-move? val)
  (and (>= val 0) (< val edge-length)))

;;____________________________________________________________

; can't get this to be part of the rules. could use let with binding??
(define finish-line 10)

;; staggering around to a goal not using the actor struct,
;;  but simple state literal lists: '(start i j) 
(define-ruleset walk-rules)

;; (state-label i j ID)

;; start state for predicate states
(define-rule (off-you-go walk-rules)
  (?st <- (start ?ID))
  ==>
  (let ([newI (random-start)]
        [newJ (random-start)])
    (printf "~a starts attttttttttt: ~a ~a\n" ?ID newI newJ) 
    (assert `(walk ,newI ,newJ ,?ID))
    (retract ?st)))

;; if they're in the same spot, get rid of the first one
(define-rule (kill-it walk-rules)                                 ;; doesn't want to do this one
  (?die <- (walk ?i-d ?j-d ?ID-d))
  (?kill <- (walk ?i-k ?j-k ?ID-k))
  (eq? ?i-d ?i-k)
  (eq? ?j-d ?j-k)
  (not (eq? ?ID-d ?ID-k))
  ==>
  (retract ?die)
  (printf "~a killed ~a!!!!!!!!!!!!!!!!!!!\n" ?ID-k ?ID-d)
  (succeed))

;; walking around within edge-length x edge-length
(define-rule (just-walking walk-rules)                                   ;; seems to be stuck here
  ;; not yet to finish line at: [40, ?]
  (?w <- (walk (?i (< ?i 1000)) ?j ?ID))
  ==>
  (let loop () 
    (let ([newI (+ ?i (rand-move))]
          [newJ (+ ?j (rand-move))])
       (if (and (valid-move? newI) (valid-move? newJ))
         (begin 
           (replace ?w `(walk ,newI ,newJ ,?ID))
           (printf "~a walks to: ~a, ~a\n" ?ID newI newJ))
         (loop)))))

;; made it
(define-rule (made-it walk-rules)
  (?w <- (walk (?i (>= ?i 1000)) ?j ?ID))
  ==>
  (printf "~a FINISHED atttttttttttttttttt: ~a ~a\n" ?ID ?i ?j))


;; Zombie Apoc. Graphics
(define cell-glyph-size 12)

(define frame (new frame% 
                   [label "Example"]
                   [width 300]
                   [height 300]))

(new canvas% [parent frame]
             [paint-callback 
              (lambda (canvas dc)
                (send dc clear)
                (send dc set-brush "blue" 'solid)
                (send dc draw-ellipse 1 2 cell-glyph-size cell-glyph-size))])
                
(define (race-to-finish)
  (with-new-inference-environment
   (current-inference-strategy 'breadth)
   (activate walk-rules)
   ;; start state.... where we could put our randomly made zombies
   (assert '(start 1))
   (assert '(start 2))
   (assert '(start 3))
   (start-inference)))

;; run it
(random-source-randomize! (current-random-source))
(send frame show #t)
(race-to-finish)
  
