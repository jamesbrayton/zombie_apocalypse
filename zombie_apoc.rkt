#lang racket/gui

;;; Nic Young | Andrew Hoyle | James Brayton

(require (planet williams/simulation/simulation)
         (planet williams/inference/inference)
         (planet williams/science/random-distributions)
         (planet williams/science/math)
         (planet williams/animated-canvas/animated-canvas))

;; __________________________GLOBALS__________________________________

(define edge-length 400)

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

;; info floating around:
;; ('actor label i j ID time strength speed)                        ;; speed not yet implemented
;; - label : 'zombie | 'person
;; (player-count #)
;; (time #)

;; gotta use 'order search procedure I think so the priorities can be set
; goals
; start
; battle
; walk
; time inc

;; staggering around to a goal not using the actor struct,
;; but simple state literal lists: '(start i j)
(define-ruleset zombie-game-rules)

;; ---------GOALS

;; GOAL state time limit
(define-rule (time-limit zombie-game-rules)
  (?timer <- (time (?t (>= ?t 1000))))                   ;; set time limit here!
  ==>
  (printf "TIME LIMIT REACHED: ~a\n" ?t)
  (succeed))

;; GOAL state one remains
(define-rule (one-left zombie-game-rules)
  (?pc <- (player-count (?c (= 1 ?c))))
  (?actor <- (actor ?label ?i ?j ?ID ?t ?str))
  ==>
  (printf "~a WINS THE GAME\n" ?ID)
  (succeed))

;; ---------START

;; START state for predicate states-randomly assigns a valid starting point
;;  and a normal distribution of the player's strengths and whether they're
;;  a 'zombie or 'person
(define-rule (set-values zombie-game-rules)
  (?start <- (start ?ID))
  (?timer <- (time ?t))
  ==>
  (let ([newI (random-start)]
        [newJ (random-start)])
    (printf "TIME ~a:\t~a starts at: ~a ~a\n" (- ?t 1) ?ID newI newJ)              ;; first time -1
    (retract ?start)
    ;; randomly create a zombie or person
    (if (< 0.5 (random))                                                           ;; change z / p ratio here
        ;; normal-distribution of strength
        (begin
          (assert `(actor zombie ,newI ,newJ ,?ID ,(- ?t 1) 
                          ,(random-gaussian 100 10)))                              ;; change str dist here
          (printf "zombie made\n"))
        (begin
          (assert `(actor person ,newI ,newJ ,?ID ,(- ?t 1) 
                          ,(random-gaussian 100 10)))                              ;; change str dist here
          (printf "person made\n")))))

;; ---------BATTLE    
    
;; zombie is stronger, and in same location as person,
;;  person is now turned to zombie and has to wait
(define-rule (zombify zombie-game-rules)
  (?new-zombie <- (actor person ?i ?j ?ID-nz ?t ?str-nz))
  (?kill <- (actor zombie ?i ?j 
                    (?ID-k (not (= ?ID-k ?ID-nz)))        ;; probably not needed
                    ?t
                    (?str-k (> ?str-k ?str-nz))))
  (?pc <- (player-count ?c))
  ==>
  (printf "TIME: ~a:\tzombie ~a zombified person ~a!\n" ?t ?ID-k ?ID-nz)
  ;; zombify person, add to their time to stall their motion                      ;; wait time is here
  (replace ?new-zombie `(actor zombie ,?i ,?j ,?ID-nz ,(+ ?t 3) ,?str-nz))        ;; replace used
  ;; player count lowered
  (retract ?pc))

;; person is stronger, and in same location as zombie
(define-rule (decapitate zombie-game-rules)
  (?die <- (actor zombie ?i ?j ?ID-d ?t ?str-d))
  (?kill <- (actor person ?i ?j 
                    (?ID-k (not (= ?ID-k ?ID-d)))        ;; probably not needed
                    ?t
                    (?str-k (> ?str-k ?str-d))))
  (?pc <- (player-count ?c))
  ==>
  (printf "TIME: ~a:\tperson ~a killed zombie ~a!\n" ?t ?ID-k ?ID-d)
  ;; killed off zombie
  (retract ?die)
  ;; player count lowered
  (retract ?pc)
  (assert `(player-count ,(- ?c 1))))

;; ---------WALKING

;; walking around within edge-length X edge-length
(define-rule (random-walking zombie-game-rules)
  (?timer <- (time ?t))
  (?actor <- (actor ?label ?i ?j ?ID (?t-a (< ?t-a ?t)) ?str))          ;; ?label can be 'zombie or 'person
  ==>
  ;; doesn't leave loop until a valid move is made inside the board
  (let loop ()                                                          ;; goes until a valid move is made
    (let ([newI (+ ?i (rand-move))]
          [newJ (+ ?j (rand-move))])
       (if (and (valid-move? newI) (valid-move? newJ))                  ;; put a wall check here too
         (begin 
           (printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)  ;; should be ?t-a
           (retract ?actor)
           (assert `(actor ,?label ,newI ,newJ ,?ID ,(+ 1 ?t-a) ,?str)))       ;; inc actor time to show it's moved
         (loop)) ;; inc actor time to show it's moved
         (let* ((dc (send canvas get-dc))
            (width (send canvas get-width))
            (height (send canvas get-height))) 
      (send dc draw-ellipse newI newJ 10 10)))))

;; ---------TIME INCREMENT

;; increment timer only when there's nothing left to do...
;;  meaning all players have moved and interacted
(define-rule (time-inc zombie-game-rules)
  (?timer <- (time ?t))
  ==>
  (retract ?timer)
  (assert `(time ,(+ 1 ?t)))
  (printf "TIME was: ~a\n\n" ?t)
  (send canvas swap-bitmaps))

;______________________________________________________________

;; All the GFX
;;____________________________________________________________________________________
(define frame
  (instantiate frame% ("Zombie Apocalypse")))

(define canvas
  (instantiate animated-canvas%
    (frame)
    (style `(border))
    (min-width 400)
    (min-height 600)))

;;____________________________________________________________________________________

;_____________________________RUN IT___________________________________________________

(define (run-zombie-sim)
  (with-new-inference-environment
   ; needed for top to bottom ordering of the rules
   (current-inference-strategy 'order)
   (activate zombie-game-rules)
   ;; start state.... where we could put our randomly made zombies
   (assert '(start 1))
   (assert '(start 2))
   (assert '(start 3))
   (assert '(start 4))
   (assert '(start 5))
   (assert '(start 6))
   (assert '(start 7))
   (assert '(start 8))
   (assert '(start 9))
   (assert '(start 10))
   (assert '(time 1))                        ;; timer keeps track of the turns
   (assert '(player-count 3))                ;; for an end state to see winner
   (start-inference)))

; randomize source
(random-source-randomize! (current-random-source))
;run it
(send frame show #t)
(run-zombie-sim)