#lang racket/gui

;;; Nic Young | Andrew Hoyle | James Brayton

(require (planet williams/simulation/simulation)
         (planet williams/inference/inference)
         (planet williams/science/random-distributions)
         (planet williams/science/math)
         (planet williams/animated-canvas/animated-canvas))

;; __________________________GLOBALS__________________________________

(define edge-length 400)
(define players 5)

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

;; picks direction to move, in radians [0, 2pi)
(define (rand-angle)
  (* (random) (* 2 pi)))

;; trig used for y, y-dist = speed * sin(theta)
(define (rand-move-trig-Y actor-speed angle-rads)
  (* actor-speed (sin angle-rads)))

;; trig used for x
(define (rand-move-trig-X actor-speed angle-rads)
  (* actor-speed (cos angle-rads)))

;; calculate angle between zombie and person in rads, 
;;  minute chance it could have div-by-zero error
(define (point-to-person iz jz ip jp)
  (if (= (- iz ip) 0)
      (atan (/ (- jz jp) 0.0000001))
      (atan (/ (- jz jp) (- iz ip)))))

;; brush coloration RGB based on strength, if str is too high 
;;  (> 255), coloration won't be valid
(define (brush-color label str)
  (cond
    ;; We have to add a little fudge to each color to get better colors
    [(equal? label 'zombie) (make-color (+ 50 (inexact->exact (round str))) 0  0 1.0)]
    [else (make-color 0 0 (+ 50 (inexact->exact (round str))) 1.0)]))
;;____________________________________________________________________

;;______________________________LOGIC_________________________________

;; info floating around:
;; ('actor label i j ID time strength speed)
;;  label : 'zombie | 'person
;; (player-count #)
;; (time #)

;; gotta use 'order search procedure I think so the priorities can be set
; goals
; start
; battle
; walk
; draw dead
; time inc

;; staggering around to a goal not using the actor struct,
;; but simple state literal lists: '(start i j)
(define-ruleset zombie-game-rules)

;; ---------GOALS

;; GOAL state time limit
(define-rule (time-limit zombie-game-rules)
  (?timer <- (time (?t (>= ?t +inf.0))))                                               ;; set time limit here------
  ==>
  (printf "TIME LIMIT REACHED: ~a\n" ?t)
  (succeed))

;; GOAL state one remains
(define-rule (one-left zombie-game-rules)
  (?pc <- (player-count (?c (= 1 ?c))))
  (?actor <- (actor ?label ?i ?j ?ID ?t ?str ?sp))
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
    ;(printf "TIME ~a:\t~a starts at: ~a ~a\n" (- ?t 1) ?ID newI newJ)              
    (retract ?start)
    ;; randomly create a zombie or person
    (if (< 0.5 (random))                                                          ;; change z / p ratio here--------
        ;; normal-distribution of strength, starts at time -1
        (begin
          (assert `(actor zombie ,newI ,newJ ,?ID ,(- ?t 1) 
                          ,(random-gaussian 100 15)                               ;; change str dist here-----------
                          ,(random-gaussian 2 0.3)))                              ;; change sp dist here------------
          ;(printf "zombie made\n")
          )
        (begin
          (assert `(actor person ,newI ,newJ ,?ID ,(- ?t 1) 
                          ,(random-gaussian 100 15)                               ;; change str dist here-----------
                          ,(random-gaussian 2 0.3)))                              ;; change sp dist here------------
          ;(printf "person made\n")
          ))))

;; ---------BATTLE    
    
;; both decapitate and zombify
(define-rule (close-enough-to-battle zombie-game-rules)
  ;; both zombie and person are in same time slice
  (?zombie <- (actor zombie ?i-z ?j-z ?ID-z ?t ?str-z ?sp-z))
  ;; AND they're close enough
  (?person <- (actor
               person 
               ?i-p
               ;; distance function
               (?j-p (> 8 (sqrt (+ (expt (- ?i-z ?i-p) 2)                    ;; change battle distance here------
                     (expt (- ?j-z ?j-p) 2)))))
               ?ID-p 
               ?t 
               ?str-p
               ?sp-p))
  ;; this will -- if decapitate is called
  (?pc <- (player-count ?c))
  ==>
  ;; compare strengths to decide if decapitate or zombify
  (if (< ?str-z ?str-p)
      ;; decapitate zombie
      (begin 
        (retract ?zombie)
        ;(printf "TIME: ~a:\tperson ~a decapitated zombie ~a!\n" ?t ?ID-p ?ID-z)
        ;; lower player count by one
        (retract ?pc)
        (assert `(player-count ,(- ?c 1))))
      ;; zombify person, add time to it so it can't interact
      (begin
        ;(printf "TIME: ~a:\tzombie ~a zombified person ~a!\n" ?t ?ID-z ?ID-p)
        (retract ?person)
        (assert `(actor zombie ,?i-p ,?j-p ,?ID-p ,(+ ?t 90) ,?str-p ,?sp-p)))))          ;; change death delay time here-------

;; ---------WALKING

;; zombie sees person, walks toward them... this must change when
;;  walls are placed because a move could not be valid
(define-rule (in-line-of-sight zombie-game-rules)
  ;; timer needed
  (?timer <- (time ?t))
  ;; zombie and person are in same time slice ?T that's less than ?t 
  (?zombie <- (actor zombie ?i-z ?j-z ?ID-z (?T (< ?T ?t)) ?str-z ?sp-z))
  ;; AND they're close enough for sight
  (?person <- (actor
               person
               ?i-p
               ;; distance function
               (?j-p (> 100 (sqrt (+ (expt (- ?i-z ?i-p) 2)                                   ;; change sight distance here------
                     (expt (- ?j-z ?j-p) 2)))))
               ?ID-p
               ?T 
               ?str-p
               ?sp-p))
  ==>
  ;; try to move toward person
  (let* ([to-person (point-to-person ?i-z ?j-z ?i-p ?j-p)]
         [newI (+ ?i-z (rand-move-trig-X ?sp-z to-person))]
         [newJ (+ ?j-z (rand-move-trig-Y ?sp-z to-person))])
    ;(printf "seen\n")
    ;(printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)
    (retract ?zombie)
    ;; inc actor time to show it's moved
    (assert `(actor zombie ,newI ,newJ ,?ID-z ,(+ 1 ?T) ,?str-z ,?sp-z))
    ;; drawing zombies in new location
    (let* ((dc (send canvas get-dc))
           (width (send canvas get-width))
           (height (send canvas get-height)))
      (send dc set-brush (brush-color 'zombie ?str-z) 'solid)        
      (send dc draw-ellipse newI newJ 10 10))))     

;; walking around within edge-length X edge-length
(define-rule (random-walking zombie-game-rules)
  (?timer <- (time ?t))
  (?actor <- (actor ?label ?i ?j ?ID (?t-a (< ?t-a ?t)) ?str ?sp))
  ==>
  ;; doesn't leave loop until a valid move is made inside the board
  (let loop ()                         
    (let* ([rand-dir (rand-angle)]
           [newI (+ ?i (rand-move-trig-X ?sp rand-dir))]
           [newJ (+ ?j (rand-move-trig-Y ?sp rand-dir))])
      (if (and (valid-move? newI) (valid-move? newJ))                              ;; put a wall check here too
          ;; new position is on the board
          (begin 
            ;(printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)
            (retract ?actor)
            ;; inc actor time to show it's moved
            (assert `(actor ,?label ,newI ,newJ ,?ID ,(+ 1 ?t-a) ,?str ,?sp)))       
          ;; new position is OFF the board, try again
          (begin
            (printf "TIME ~a:\t~a ~a failed to walk from (~a, ~a)\n" ?t-a ?label ?ID newI newJ)
            (loop)))
      ;; drawing actors in new location
      (let* ((dc (send canvas get-dc))
             (width (send canvas get-width))
             (height (send canvas get-height)))
        (send dc set-brush (brush-color ?label ?str) 'solid)        
        (send dc draw-ellipse newI newJ 10 10)))))

;; ---------DRAW RULE

;; Draw the dead people b/c they're in a future time slice after zombification
(define-rule (draw-dead zombie-game-rules)
  (?timer <- (time ?t))
  (?actor <- (actor ?label ?i ?j ?ID (?t-a (> ?t-a ?t)) ?str ?sp))
  ==>
   (let* ((dc (send canvas get-dc))
          (width (send canvas get-width))
          (height (send canvas get-height)))
     ;; dead are grey
     (send dc set-brush (make-color 0 0 0 .5) 'solid)
     (send dc draw-ellipse ?i ?j 10 10)))

;; ---------TIME INCREMENT

;; increment timer only when there's nothing left to do...
;;  meaning all players have moved and interacted
(define-rule (time-inc zombie-game-rules)
  (?timer <- (time ?t))
  ==>
  (retract ?timer)
  (assert `(time ,(+ 1 ?t)))
  ;(printf "TIME was: ~a\n\n" ?t)
  (send canvas swap-bitmaps))

;______________________________________________________________

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
(send frame show #t)
(run-zombie-sim)
