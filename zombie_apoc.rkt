#lang racket

(require (planet williams/science/random-distributions))
(require (planet williams/science/math))
(require (planet williams/simulation/simulation)
         (planet williams/inference/inference))

;; __________________________GLOBALS__________________________________

(define edge-length 10)

;;____________________________________________________________________

;;_______________________NOT USED YET_________________________________

;; struct to hold people/zombies
(define-struct actor (i j type strength speed IQ)) 

;; calculates distance between actors
(define (distancer a1 a2)
  (sqrt (+ (expt (- (actor-i a1) (actor-i a2)) 2)
                (expt (- (actor-j a1) (actor-j a2)) 2))))

;;_____________________________________________________________________


;;____________________________predicate functions______________________

;; walk that just is [-1,1], apply to i and j
(define (rand-move)
  (+ (random-integer 3) -1))

;; pick a random spot within the bounds to start
(define (random-start)
  (random-integer edge-length))                       ;; uses edge-length GLOBAL

;; within the bounds? 
(define (valid-move? val)
  (and (>= val 0) (< val edge-length)))               ;; uses edge-length GLOBAL

;;____________________________________________________________________

;;______________________________LOGIC_________________________________

; can't get this to be part of the rules. could use let with binding??
(define finish-line 10)

;; info floating around:
;; (state-label i j ID time strength speed)
;; - state label : 'zombie 'person
;; (player-count #)
;; (time #)

;; gotta use 'order search procedure I think so the priorities can be set
; goals
; start
; battle
; battleCheck
; walk
; time inc

;; staggering around to a goal not using the actor struct,
;; but simple state literal lists: '(start i j)
(define-ruleset walk-rules)

;; GOAL state time limit
(define-rule (time-limit walk-rules)
  (?timer <- (time (?t (>= ?t 100))))                   ;;set time limit here!
  ==>
  (printf "TIME LIMIT REACHED: ~a\n" ?t)
  (succeed))

;; GOAL state one remains
(define-rule (one-left walk-rules)
  (?pc <- (player-count (?c (= 1 ?c))))
  (?w <- (walk ?i ?j ?ID ?t))
  ==>
  (printf "~a WINS THE GAME\n" ?ID)
  (succeed))

;; START state for predicate states-randomly assigns a valid starting point
;;  and a normal distribution of the player's strengths and whether they're
;;  a 'zombie or 'person
(define-rule (set-values walk-rules)
  (?st <- (start ?ID))
  (?timer <- (time ?t))
  ==>
  (let ([newI (random-start)]
        [newJ (random-start)])
    (printf "TIME ~a:\t~a starts at: ~a ~a\n" (- ?t 1) ?ID newI newJ)      ;; first time -1
    (retract ?st)
    (assert `(walk ,newI ,newJ ,?ID ,(- ?t 1)))))

;; if they're in the same spot, get rid of the first one, 
;;  must be SAME time and different IDs
(define-rule (battle-time walk-rules)
  (?die <- (walk ?i ?j ?ID-d ?t-d))
  (?kill <- (walk ?i ?j (?ID-k (not (= ?ID-k ?ID-d))) (?t-k (= ?t-d ?t-k))))
  (?pc <- (player-count ?c))
  ==>
  (printf "TIME: ~a:\t~a killed ~a!!!!!!!!!!!!!!!!!!!\n" ?t-d ?ID-k ?ID-d)
  (retract ?die)
  (retract ?pc)
  (assert `(player-count ,(- ?c 1))))

;; walking around within edge-length x edge-length
(define-rule (just-walking walk-rules)
  ;; not yet to finish line at: [40, ?]
  (?timer <- (time ?t))
  (?w <- (walk ?i ?j ?ID (?tw (< ?tw ?t))))
  ==>
  ;; doesn't leave loop until a valid move is made
  (let loop ()                                                 ;; goes until a valid move is made
    (let ([newI (+ ?i (rand-move))]
          [newJ (+ ?j (rand-move))])
       (if (and (valid-move? newI) (valid-move? newJ))         ;; put a wall check here too
         (begin 
           (printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)
           (retract ?w)
           (assert `(walk ,newI ,newJ ,?ID ,(+ 1 ?tw))))       ;; inc actor time to show it's moved
         (loop)))))

;; increment timer only when there's nothing left to do...
;;  meaning all players have moved and interacted
(define-rule (time-forward walk-rules)
  (?timer <- (time ?t))
  ==>
  (retract ?timer)
  (assert `(time ,(+ 1 ?t)))
  (printf "TIME was: ~a\n\n" ?t))

;______________________________________________________________

;_____________________________RUN IT___________________________________________________

(define (race-to-finish)
  (with-new-inference-environment
   ; needed for top to bottom ordering of the rules
   (current-inference-strategy 'order)
   (activate walk-rules)
   ;; start state.... where we could put our randomly made zombies
   (assert '(start 1))
   (assert '(start 2))
   (assert '(start 3))
   (assert '(time 1))                        ;; timer keeps track of the turns
   (assert '(player-count 3))                ;; for an end state to see winner
   (start-inference)))

; randomize source
(random-source-randomize! (current-random-source))
;run it
(race-to-finish)
