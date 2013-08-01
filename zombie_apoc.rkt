#lang racket/gui

;;; Nic Young | Andrew Hoyle | James Brayton

(require (planet williams/simulation/simulation)
         (planet williams/inference/inference)
         (planet williams/science/random-distributions)
         (planet williams/science/math)
         (planet williams/animated-canvas/animated-canvas))

;; __________________________GLOBALS AND STRUCTS______________________

(define EDGE-LENGTH 600)
(define PLAYERS 50)
(define WALLS 40)

;; the x and y MUST be the top left coordinates!
(struct wall (x y w l))

;; holds all of the walls to be checked for collisions
(define WALL-LST `())

;;____________________________________________________________________


;;____________________________predicate functions______________________





;;------------------------WALL STUFF

;; checks to see if actor is randomly placed inside any of the walls
(define (valid-start? i j wall-lst)
  (andmap (lambda (w) (not (in-wall? i j w))) wall-lst))

;; actor is placed in a wall?
(define (in-wall? i j w)
  (if (and (and (and (>= i (wall-x w))
                     (<= i (+ (wall-x w) (wall-w w))))
                (>= j (wall-y w)))
           (<= j (+ (wall-y w) (wall-l w))))
      #t
      #f)) 

;; not blocked by wall and not of board? then #t, allowing i and j to update
(define (valid-move-LR? new-i new-j old-i old-j wall-lst edge-len)
  (andmap (lambda (w) (and (not-thru-wall-LR? new-i new-j old-i old-j w)
                           (on-board? new-i edge-len))) wall-lst))

(define (valid-move-UD? new-i new-j old-i old-j wall-lst edge-len)
  (andmap (lambda (w) (and (not-thru-wall-UD? new-i new-j old-i old-j w)
                           (on-board? new-j edge-len))) wall-lst))

;; on the space defined by EDGE-LENGTH?
(define (on-board? n el)
  (if (and (< n el) (>= n 0))
      #t
      #f))

;; all 4 directions, split up to allow movement in the direction that
;;  still works
(define (not-thru-wall-LR? ni nj oi oj wall)
  (if (and (left-okay? ni nj oi oj wall)
           (right-okay? ni nj oi oj wall))
      #t
      #f))

(define (not-thru-wall-UD? ni nj oi oj wall)
  (if (and (up-okay? ni nj oi oj wall)
           (down-okay? ni nj oi oj wall))
      #t
      #f))

;; the directions, FAIL conditions anded together
(define (left-okay? ni nj oi oj w)
  (if (and (and (and (<= nj (+ (wall-y w) (wall-l w)))
                     (>= nj (wall-y w)))
                (>= oi (+ (wall-x w) (wall-w w))))
           (<= ni (+ (wall-w w) (wall-x w))))
      #f
      #t))

(define (right-okay? ni nj oi oj w)
  (if (and (and (and (<= nj (+ (wall-y w) (wall-l w)))
                     (>= nj (wall-y w)))
                (<= oi (wall-x w)))
           (>= ni (wall-x w)))
      #f
      #t))

(define (up-okay? ni nj oi oj w)
  (if (and (and (and (<= ni (+ (wall-x w) (wall-w w)))
                     (>= ni (wall-x w)))
                (>= oj (+ (wall-y w) (wall-l w))))
           (<= nj (+ (wall-y w) (wall-l w))))
      #f
      #t))

(define (down-okay? ni nj oi oj w)
  (if (and (and (and (<= ni (+ (wall-x w) (wall-w w)))
                     (>= ni (wall-x w)))
                (<= oj (wall-y w)))
           (>= nj (wall-y w)))
      #f
      #t))

;; randomly generates walls
(define (random-len)
  (random-integer 100))                                                                                 ;; change wall here------
(define (random-wid)
  (random-integer 100))                                                                                ;; change wall here------

(define (random-x-y)
  (random-integer EDGE-LENGTH))
  
;; stuff random walls into GLOBAL WALL-LST
(define (add-walls! w)
  (set! WALL-LST (append WALL-LST (list (wall (random-x-y) (random-x-y) (random-wid) (random-len))))))

;;---------------------

;; pick a random spot within the bounds to start
(define (random-start)
  (random-integer EDGE-LENGTH))                       ;; uses EDGE-LENGTH GLOBAL

;; picks direction to move, in radians [0, 2pi)
(define (rand-angle)
  (* (random) (/ pi 2)))

;; pick x and y quadrants randomly, y-dir = speed * sin(theta)
(define (rand-move-Y speed rand-corner)
  (if (< 0.5 (random))
      (* speed (sin rand-corner))
      (* (* speed -1) (sin rand-corner))))

;; pick x and y quadrants randomly, x-dir = speed * cos(theta)
(define (rand-move-X speed rand-corner)
  (if (< 0.5 (random))
      (* speed (cos rand-corner))
      (* (* speed -1) (cos rand-corner))))

;; -----------SEARCHING STUFF

;; calculate angle between zombie and person in rads, 
;;  minute chance it could have div-by-zero error
(define (point-to-person iz jz ip jp)
  (if (= (- ip iz) 0)
      ;; prevents DIV-BY-ZERO error
      (abs (atan (/ (- jp jz) 0.00000000001)))
      (abs (atan (/ (- jp jz) (- ip iz))))))

;; takes zombie info when it sees person and gives new movement value
;;  in X direction based on relative quadrant person is to it
(define (point-move-X speed iz jz ip jp)
  (let* ([angle (point-to-person iz jz ip jp)])
    (case (quad-calc (- ip iz) (- jp jz))
      [(1) (* speed (cos angle))]
      [(2) (* speed (cos (- pi angle)))]
      [(3) (* speed (cos (+ pi angle)))]
      [(4) (* speed (cos (- (* 2 pi) angle)))])))

;; takes zombie info when it sees person and gives new movement value
;;  in Y direction based on relative quadrant person is to it
(define (point-move-Y speed iz jz ip jp)
  (let* ([angle (point-to-person iz jz ip jp)])
    (case (quad-calc (- ip iz) (- jp jz))
      [(1) (* speed (sin angle))]
      [(2) (* speed (sin (- pi angle)))]
      [(3) (* speed (sin (+ pi angle)))]
      [(4) (* speed (sin (- (* 2 pi) angle)))])))

;; calcs quadrant based on x y values
(define (quad-calc i j)
  (case `(,(<= 0 i) ,(<= 0 j))
    ['(#t #t) 1]
    ['(#f #t) 2]
    ['(#f #f) 3]
    ['(#t #f) 4]))

;; -------------PAINTING

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
; random walls
; start
; battle
; walk
; draw dead
; time inc / draw walls / blit screen

;; staggering around to a goal not using the actor struct,
;; but simple state literal lists: '(start i j)
(define-ruleset zombie-game-rules)

;; ---------GOALS

;; GOAL state time limit
(define-rule (time-limit zombie-game-rules)
  (?timer <- (time (?t (>= ?t 10000))))                                               ;; set time limit here------
  ==>
  (printf "TIME LIMIT REACHED: ~a\n" ?t)
  (succeed))

;; GOAL state one remains
(define-rule (one-left zombie-game-rules)
  (?pc <- (player-count (?c (= 1 ?c))))
  (?actor <- (actor ?label ?i ?j ?ID ?t ?str ?sp))
  ==>
  (printf "~a KILLED EVERYONE!\n" ?ID)
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

;; Randomly Generate the walls
;; This needs to happen before the actors are constructed
(define-rule (set-walls zombie-game-rules)
  (?gen-walls <- (gen-walls ?ID))
  ==>
  (retract ?gen-walls)
  (add-walls! 0))

;; START state for predicate states-randomly assigns a valid starting point
;;  and a normal distribution of the player's strengths and whether they're
;;  a 'zombie or 'person
(define-rule (set-values zombie-game-rules)
  (?start <- (start ?ID))
  (?timer <- (time ?t))
  ==>
  (let ([newI (random-start)]
        [newJ (random-start)])
    ;; checks for valid placement, if not, falls through
    (when (valid-start? newI newJ WALL-LST)
        (begin          
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
                ))))))

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
               (?j-p (> 9 (sqrt (+ (expt (- ?i-z ?i-p) 2)                    ;; change battle distance here------
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
        (assert `(actor zombie ,?i-p ,?j-p ,?ID-p ,(+ ?t 200) ,?str-p ,?sp-p)))))          ;; change death delay time here-------

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
               (?j-p (> 60 (sqrt (+ (expt (- ?i-z ?i-p) 2)                                   ;; change sight distance here------
                     (expt (- ?j-z ?j-p) 2)))))
               ?ID-p
               ?T 
               ?str-p
               ?sp-p))
  ==>
  ;; try to move toward person
  (let* ([I (+ ?i-z (point-move-X ?sp-z ?i-z ?j-z ?i-p ?j-p))]
         [J (+ ?j-z (point-move-Y ?sp-z ?i-z ?j-z ?i-p ?j-p))]
         [newI (if (valid-move-LR? I J ?i-z ?j-z WALL-LST EDGE-LENGTH) I ?i-z)]      
         [newJ (if (valid-move-UD? I J ?i-z ?j-z WALL-LST EDGE-LENGTH) J ?j-z)])
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
      (send dc draw-ellipse (- newI 5) (- newJ 5) 10 10))))     

;; walking around within EDGE-LENGTH X EDGE-LENGTH
(define-rule (random-walking zombie-game-rules)
  (?timer <- (time ?t))
  (?actor <- (actor ?label ?i ?j ?ID (?t-a (< ?t-a ?t)) ?str ?sp))
  ==>
  ;; set I/J back to ?i/?j if they're over an edge boundary          
  (let* ([rand-dir (rand-angle)]
         [I (+ ?i (rand-move-X ?sp rand-dir))]
         [J (+ ?j (rand-move-Y ?sp rand-dir))]
         [newI (if (valid-move-LR? I J ?i ?j WALL-LST EDGE-LENGTH) I ?i)]       
         [newJ (if (valid-move-UD? I J ?i ?j WALL-LST EDGE-LENGTH) J ?j)])
    ;(printf "TIME ~a:\t~a walks to: ~a, ~a\n" ?t ?ID newI newJ)
    (retract ?actor)
    ;; inc actor time to show it's moved
    (assert `(actor ,?label ,newI ,newJ ,?ID ,(+ 1 ?t-a) ,?str ,?sp))       
    ;; drawing actors in new location
    (let* ((dc (send canvas get-dc))
           (width (send canvas get-width))
           (height (send canvas get-height)))
      (send dc set-brush (brush-color ?label ?str) 'solid)        
      (send dc draw-ellipse (- newI 5) (- newJ 5) 10 10))))

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
     (send dc draw-ellipse (- ?i 5) (- ?j 5) 10 10)))

;; ---------TIME INCREMENT AND WALL DRAWING

;; increment timer only when there's nothing left to do...
;;  meaning all PLAYERS have moved and interacted
(define-rule (time-inc-draw-walls zombie-game-rules)
  (?timer <- (time ?t))
  ==>
  (retract ?timer)
  (assert `(time ,(+ 1 ?t)))
  ;(printf "TIME was: ~a\n\n" ?t)
  ;; ALSO, draw the walls as rectangles
  (for ([w WALL-LST])                            ;; uses WALL-LST GLOBAL
    (let* ((dc (send canvas get-dc))  
           (width (send canvas get-width))
           (height (send canvas get-height)))
       ;; walls' color settings
       (send dc set-brush (make-color 10 20 0 .75) 'solid)
       (send dc draw-rectangle (wall-x w) (wall-y w) (wall-w w) (wall-l w))))
  ;; blit
  (send canvas swap-bitmaps))

;______________________________________________________________
  
;_____________________________SIM _____________________________

(define (run-zombie-sim)
  (with-new-inference-environment
   ;; needed for top to bottom ordering of the rules
   (current-inference-strategy 'order)
   (activate zombie-game-rules)
   ;; timer keeps track of the turns
   (assert '(time 1))                        
   ;; for an end state to see winner
   (assert `(player-count ,PLAYERS))
   ;; generate random walls
   (for ((i (in-range WALLS)))
     (assert `(gen-walls, i)))
   ;; create the PLAYERS based on PLAYERS GLOBAL
   (for ((i (in-range PLAYERS)))
     (assert `(start ,i)))
   (start-inference)))

;;__________________________FRAME_______________________________
(define frame
  (instantiate frame% ("Zombie Apocalypse")))

(define canvas
  (instantiate animated-canvas%
    (frame)
    (style `(border))
    (min-width 600)
    (min-height 650)))

(define tool-bar (instantiate horizontal-panel% (frame)
                   (alignment '(right center))))

;; runs the game when pressed
(define start-button (instantiate button%
                       ("Start"
                        tool-bar
                        (lambda (button event)
                          (run-zombie-sim)))))

;;______________________________________________________________

;; randomize source
(random-source-randomize! (current-random-source))
;; show frame
(send frame show #t)







