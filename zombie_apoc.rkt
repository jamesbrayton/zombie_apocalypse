#lang racket

;;; Nic Young | Andrew Hoyle | James Brayton

(require (planet williams/simulation/simulation)
         (planet williams/inference/inference)
         (planet williams/science/random-distributions)
         (planet williams/science/math))

;; __________________________GLOBALS__________________________________

(define edge-length 10)
(define players 15)

;;____________________________________________________________________


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
  (?timer <- (time (?t (>= ?t 100))))                                            ;; set time limit here!---------
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

;; GOAL state only people remain
(define-rule (people-win zombie-game-rules)
  (no (actor zombie . ?))
  ==>
  (printf "PEOPLE SURVIVED APOCALYPSE!!!")
  (succeed))

;; GOAL state only zombies remain
(define-rule (zombies-win zombie-game-rules)
  (no (actor person . ?))
  ==>
  (printf "ZOMBIES TAKE OVER!!!")
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
    (printf "TIME ~a:\t~a starts at: ~a ~a\n" (- ?t 1) ?ID newI newJ)              
    (retract ?start)
    ;; randomly create a zombie or person
    (if (< 0.5 (random))                                                           ;; change z / p ratio here--------
        ;; normal-distribution of strength, starts at time -1
        (begin
          (assert `(actor zombie ,newI ,newJ ,?ID ,(- ?t 1) 
                          ,(random-gaussian 100 10)))                              ;; change str dist here-----------
          (printf "zombie made\n"))
        (begin
          (assert `(actor person ,newI ,newJ ,?ID ,(- ?t 1) 
                          ,(random-gaussian 100 10)))                              ;; change str dist here------------
          (printf "person made\n")))))

;; ---------BATTLE    
    
;; both decapitate and zombify
(define-rule (close-enough-to-battle zombie-game-rules)
  ;; both zombie and person are in same time slice
  (?zombie <- (actor zombie ?i-z ?j-z ?ID-z ?t ?str-z))
  ;; AND they're close enough
  (?person <- (actor person ?i-p
                     (?j-p (> 5 (sqrt (+ (expt (- ?i-z ?i-p) 2)                    ;; change battle distance here------
                           (expt (- ?j-z ?j-p) 2)))))
                     ?ID-p ?t ?str-p))
  (?pc <- (player-count ?c))
  ==>
  ;; compare strengths to decide if decapitate or zombify
  (if (< ?str-z ?str-p)
      ;; decapitate zombie
      (begin 
        (retract ?zombie)
        (printf "TIME: ~a:\tperson ~a decapitated zombie ~a!\n" ?t ?ID-p ?ID-z)
        ;; lower player count by one
        (retract ?pc)
        (assert `(player-count ,(- ?c 1))))
      ;; zombify person, add time to it so it can't interact
      (begin
        (printf "TIME: ~a:\tzombie ~a zombified person ~a!\n" ?t ?ID-z ?ID-p)
        (retract ?person)
        (assert `(actor zombie ,?i-p ,?j-p ,?ID-p ,(+ ?t 3) ,?str-p)))))

;; ---------WALKING

;; walking around within edge-length X edge-length
(define-rule (random-walking zombie-game-rules)
  (?timer <- (time ?t))
  (?actor <- (actor ?label ?i ?j ?ID (?t-a (< ?t-a ?t)) ?str))
  ==>
  ;; doesn't leave loop until a valid move is made inside the board
  (let loop ()                         
    (let ([newI (+ ?i (rand-move))]
          [newJ (+ ?j (rand-move))])
       (if (and (valid-move? newI) (valid-move? newJ))                  ;; put a wall check here too
         (begin 
           (printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)
           (retract ?actor)
           ;; ++ actor time to show it's moved
           (assert `(actor ,?label ,newI ,newJ ,?ID ,(+ 1 ?t-a) ,?str)))       
         (loop)))))

;; ---------TIME INCREMENT

;; increment timer only when there's nothing left to do...
;;  meaning all players have moved and interacted
(define-rule (time-inc zombie-game-rules)
  (?timer <- (time ?t))
  ==>
  (retract ?timer)
  (assert `(time ,(+ 1 ?t)))
  (printf "TIME was: ~a\n\n" ?t))

;______________________________________________________________

;_____________________________RUN IT___________________________

(define (run-zombie-sim)
  (with-new-inference-environment
   ; needed for top to bottom ordering of the rules
   (current-inference-strategy 'order)
   (activate zombie-game-rules)
   ;; timer keeps track of the turns
   (assert '(time 1))                        
   ;; for an end state to see winner
   (assert `(player-count ,players))                
   ;; create the players based on players GLOBAL
   (for ((i (in-range players)))
     (assert `(start ,i)))
   (start-inference)))

; randomize source
(random-source-randomize! (current-random-source))
;run it
(run-zombie-sim)
