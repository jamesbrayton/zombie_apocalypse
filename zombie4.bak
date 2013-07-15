#lang racket

;; 7/13/13
;; why do the guys "kill" themselves if there's retract/assert and then not do anything if replace?


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

;; info floating around:
;; (state-label i j ID time)
;; (player-count #)
;; (time #)

;; gotta use 'order search procedure I think so the priorities can be set
; goal
; start
; battle
; battleCheck
; walk

;; staggering around to a goal not using the actor struct,
;;  but simple state literal lists: '(start i j) 
(define-ruleset walk-rules)  

;; GOAL state time limit
(define-rule (time-limit walk-rules)
  (?timer <- (time (?t (>= ?t 100))))
  ==>
  (printf "TIME LIMIT REACHED: ~a\n" ?t)
  (succeed))

;; START state for predicate states-randomly assigns a valid starting point
(define-rule (set-locations-randomly walk-rules)
  (?st <- (start ?ID))
  (?timer <- (time ?t))
  ==>
  (let ([newI (random-start)]
        [newJ (random-start)])
    (printf "TIME ~a:\t~a starts at: ~a ~a\n" (- ?t 1) ?ID newI newJ)      ;first time -1
    (retract ?st)
    (assert `(walk ,newI ,newJ ,?ID ,(- ?t 1)))))

;; if they're in the same spot, get rid of the first one, must be SAME time
(define-rule (battle-time walk-rules)                                 
  (?die <- (walk ?i-d ?j-d ?ID-d ?t-d))
  (?kill <- (walk ?i-k ?j-k ?ID-k ?t-k))
  ==>
  (if (and (and (and (and                             
                      (= ?t-d ?t-k)              ;same time-slice?
                      (= ?i-d ?i-k)
                      (= ?j-d ?j-k)
                      (not (= ?ID-d ?ID-k))))))  ;not killing yourself?
      (begin0 (printf "TIME: ~a:\t~a killed ~a!!!!!!!!!!!!!!!!!!!\n" ?t-d ?ID-k ?ID-d)
              (retract ?die))
      (printf "TIME ~a:\t~a failed to kill ~a\n" ?t-d ?ID-k ?ID-d)))

;; walking around within edge-length x edge-length
(define-rule (just-walking walk-rules)                                   ;; seems to be stuck here
  ;; not yet to finish line at: [40, ?]
  (?timer <- (time ?t))
  (?w <- (walk ?i ?j ?ID (?tw (< ?tw ?t))))
  ==>
  (let loop ()                                                 ;; goes until a valid move is made
    (let ([newI (+ ?i (rand-move))]
          [newJ (+ ?j (rand-move))])
       (if (and (valid-move? newI) (valid-move? newJ))
         (begin 
           (printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)
           (retract ?w)
           (assert `(walk ,newI ,newJ ,?ID ,(+ 1 ?tw))))       ;; inc actor time to show it's moved
         (loop)))))

  ; increment timer
(define-rule (time-forward walk-rules)
  (?timer <- (time ?t))
  ==>
  (retract ?timer)
  (assert `(time ,(+ 1 ?t)))
  (printf "TIME was: ~a\n\n" ?t))
  
;______________________________________________________________
  
(define (race-to-finish)
  (with-new-inference-environment
   ; needed for top to bottom ordering of the rules
   (current-inference-strategy 'order)
   (activate walk-rules)
   ;; start state.... where we could put our randomly made zombies
   (assert '(start 1))
   (assert '(start 2))
   (assert '(start 3))
   (assert '(time 1))
   (assert '(count 3))
   (start-inference)))

;; run it
(random-source-randomize! (current-random-source))
(race-to-finish)
  