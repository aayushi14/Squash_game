;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname squash_game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)
(require racket/trace)

;; Squash Practice.
;; There is a list of balls and a racket. They move individually.
;; But space changes the state of the entire system from
;; ready-to-serve state to rally state.
;; In ready-to-serve state, one ball and a racket is given,
;; in stationary postion.
;; In rally state, more balls can be created by clicking "b"
;; these balls and racket are in moving position
;; When in rally state, space pauses or unpauses the entire system.

;; Moving balls.
;; The balls collide with the wall and the racket
;; Once the simulation is in rally state,
;; the balls start moving with a particular velocity

;; Moving racket.  
;; A racket hits the balls towards the wall in the scene.
;; The user can move the racket from left to right or up and down
;; with the arrow keys as well as use the mouse
;; to hit the balls with a particular velocity.

;; start with (simulation 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; Color : A color is represented by one of the strings
;; --"white"
;; --"yellow"
;; --"black"
;; --"green"
;; --"blue"
;; INTERPRETATION: Behave as colors
;; EXAMPLES:
(define white-color "white")
(define yellow-color "yellow")
(define black-color "black")
(define green-color "green")
(define blue-color "blue")

;; OBSERVER TEMPLATE:
;; color-fn : Color -> ?
;;(define (color-fn c)
;;  (cond
;;    [(= c "white")  ...]
;;    [(= c "yellow") ...]
;;    [(= c "black")  ...]
;;    [(= c "green")  ...]
;;    [(= c "blue")  ...]))

;; State : A state is represented by one of the strings
;; -- "ready-to-serve-state"
;; -- "rally-state"
;; -- "paused-state"
;; INTERPRETATION: Different states of the world
;; EXAMPLES:
(define ready-to-serve-state "ready-to-serve-state")
(define rally-state "rally-state")
(define paused-state "paused-state")

;; OBSERVER TEMPLATE:
;; state-fn : State -> ?
;;(define (state-fn s)
;;  (cond
;;  [(= s "ready-to-serve-state")  ...]
;;  [(= s "rally-state") ...]
;;  [(= s "paused-state")  ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; initial position and velocity of the ball and the racket,
;; expressed as graphics coordinates
(define BALL-IPX 330)
(define BALL-IPY 384)
(define RACKET-IPX 330)
(define RACKET-IPY 384)
(define POINTER-IPX 0)
(define POINTER-IPY 0)
(define BALL-IVX 0)
(define BALL-IVY 0)
(define RACKET-IVX 0)
(define RACKET-IVY 0)
(define BALL-RVX 3)
(define BALL-RVY -9)
(define TIMER 3)
(define POINTER-DISTANCE 25)

;; dimensions of the court
(define COURT-WIDTH 425)
(define COURT-HEIGHT 649)
(define COURT (empty-scene COURT-WIDTH COURT-HEIGHT))
(define PAUSED-COURT (empty-scene COURT-WIDTH COURT-HEIGHT yellow-color))

(define BACK-WALL-Y 649)
(define FRONT-WALL-Y 0)
(define LEFT-WALL-X 0)
(define RIGHT-WALL-X 425)

;; dimensions of the ball and racket
(define RACKET-WIDTH 47)
(define RACKET-HALF-WIDTH (floor (/ RACKET-WIDTH 2)))
(define RACKET-HEIGHT 7)
(define BALL-RADIUS 3)
(define POINTER-RADIUS 4)
(define ball-image (circle BALL-RADIUS "solid" black-color))
(define racket-image (rectangle RACKET-WIDTH RACKET-HEIGHT "solid" green-color))
(define mouse-pointer (circle POINTER-RADIUS "solid" blue-color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; REPRESENTATION:
;; A World is represented as a (make-world balls racket speed timer state)
;; INTERPRETATION:
;; balls  : BallList is a list of squash balls depicted by solid black circle
;;                   in the world
;; racket : Racket   is a player's racket depicted by a
;;                   solid green rectangle in the world
;; speed  : PosReal  is the speed in seconds in which simulation runs
;; timer  : PosInt   is the seconds that simulation takes to reset (0<timer<4)
;; state  : State    is the state of the world

;; IMPLEMENTATION:
(define-struct world (balls racket speed timer state))

;; CONSTRUCTOR TEMPLATE:
;; (make-world BallList Racket PosReal PosInt State)

;; OBSERVER TEMPLATE:
;; world-fn : World -> ??
(define (world-fn w)
  (... (world-balls w)
       (world-racket w)
       (world-speed w)
       (world-timer w)
       (world-state w)))

;; REPRESENTATION:
;; A Ball is represented as
;; (make-ball x y vx vy)
;; INTERPRETATION:
;; x, y : Integer the position of the center of the ball in the scene 
;; vx, vy : Integer the velocity of the ball in the scene

;; IMPLEMENTATION
(define-struct ball (x y vx vy))

;; CONSTRUCTOR TEMPLATE:
;; (make-ball Integer Integer Integer Integer)

;; OBSERVER TEMPLATE:
;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;; REPRESENTATION:
;; A BallList is represented as a list of Balls

;; CONSTRUCTOR TEMPLATES:
;; empty
;; (cons b blist)
;; -- WHERE
;;    b  is a Ball
;;    blist is a BallList

;; OBSERVER TEMPLATE:
;; blist-fn : BallList -> ??
#;
(define (blist-fn blist)
  (cond
    [(empty? blist) ...]
    [else (...
           (first blist)
           (blist-fn (rest blist)))]))


;; REPRESENTATION:
;; A Racket is represented as
;; (make-racket x y vx vy mx my selected?)
;; INTERPRETATION:
;; x, y   : Integer the position of the center of the racket in the scene
;; vx, vy : Integer the velocity of the racket in the scene
;; mx, my : Integer the position of the center of the pointer in the scene
;; selected? : describes whether or not the racket is selected.

;; IMPLEMENTATION
(define-struct racket (x y vx vy mx my selected?))

;; CONSTRUCTOR TEMPLATE:
;; (make-racket Integer Integer Integer Integer Integer Integer Boolean)

;; OBSERVER TEMPLATE:
;; racket-fn : Racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-mx r)
       (racket-my r)
       (rakcet-selected? r)))

;; simulation : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;        (so larger numbers run slower)
;; EFFECT: runs the simulation, starting with the initial world
;; RETURNS: the final state of the world
;; EXAMPLES: (simulation 1) runs in super slow motion
;;           (simulation 1/24) runs at a more realistic speed
;; DESIGN STRATEGY: Using the constructor template of World

(define (simulation speed)
  (big-bang (make-world (list (make-ball BALL-IPX BALL-IPY BALL-IVX BALL-IVY))
                        (make-racket RACKET-IPX RACKET-IPY
                                     RACKET-IVX RACKET-IVY
                                     POINTER-IPX POINTER-IPY
                                     false)
                        speed
                        TIMER
                        ready-to-serve-state)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

#;
(begin-for-test
  (check-equal?
   (simulation (/ 1 10))
   (make-world (list (make-ball 330 384 0 0))
               (make-racket 330 384 0 0 0 0 false) 0.1 3 ready-to-serve-state)
   "simulation failed"))

;; initial-world : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;        (so larger numbers run slower)
;; RETURNS: the ready-to-serve state of the world
;; EXAMPLE: (initial-world 1)
;; DESIGN STRATEGY: Using the constructor template of World

(define (initial-world speed)
  (make-world (list (make-ball BALL-IPX BALL-IPY BALL-IVX BALL-IVY))
              (make-racket RACKET-IPX RACKET-IPY
                           RACKET-IVX RACKET-IVY
                           POINTER-IPX POINTER-IPY false)
              speed
              TIMER
              ready-to-serve-state))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world (/ 1 10))
   (make-world (list (make-ball 330 384 0 0))
               (make-racket 330 384 0 0 0 0 false) 0.1 3 ready-to-serve-state)
   "initial-world failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-ready-to-serve? : World -> Boolean
;; GIVEN: a world
;; RETURNS: true iff the world is in its ready-to-serve state
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of World

(define (world-ready-to-serve? w)
  (and (ball-ready-to-serve? (world-balls w))
       (racket-ready-to-serve? (world-racket w))
       (= (world-timer w) TIMER)
       (string=? (world-state w) ready-to-serve-state)))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-ready-to-serve? (make-world (list (make-ball 330 384 0 0))
                                      (make-racket 330 384 0 0 0 0 false)
                                      1 TIMER ready-to-serve-state))
   #true
   "world is not ready to serve"))

;; help function for checking ball location
;; ball-ready-to-serve? : BallList -> Boolean
;; GIVEN: a list of balls which has just one ball
;; RETURNS: true if the ball is in its ready-to-serve state
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of BallList

(define (ball-ready-to-serve? blist)
  (cond
    [(empty? blist) false]
    [else (and (= (ball-x (first blist)) BALL-IPX)
               (= (ball-y (first blist)) BALL-IPY)
               (= (ball-vx (first blist)) BALL-IVX)
               (= (ball-vy (first blist)) BALL-IVY))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-ready-to-serve? (list (make-ball 330 384 0 0)))
   #true
   "ball is ready")
  (check-equal?
   (ball-ready-to-serve? (list))
   #false
   "give ball in list"))

;; help function for checking racket location
;; racket-ready-to-serve? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true if the racket is in its ready-to-serve state
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Racket

(define (racket-ready-to-serve? r)
  (and (= (racket-x r) RACKET-IPX)
       (= (racket-y r) RACKET-IPY)
       (= (racket-vx r) RACKET-IVX)
       (= (racket-vy r) RACKET-IVY)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-ready-to-serve? (make-racket 330 384 0 0 0 0 false))
   #true
   "racket is ready")
  (check-equal?
   (racket-ready-to-serve? (make-racket 330 384 5 0 0 0 false))
   #false
   "racket is not ready"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: any world that's possible for the simulation
;; RETURNS: the world that should follow the given world after a tick
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the templates of World

(define (world-after-tick w)
  (cond [(world-in-paused-state? w)
         (if (> (world-timer w) 1) (make-world (world-balls w)
                                               (world-racket w)
                                               (world-speed w)
                                               (- (world-timer w) 1)
                                               (world-state w))
             (initial-world (world-speed w)))]
        [(world-ready-to-serve? w) (initial-world (world-speed w))]
        [(world-in-rally-state? w) (world-in-rally
                                    (world-after-updating-blist w))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-tick (make-world (list (make-ball 330 384 0 0))
                                 (make-racket 330 384 0 0 0 0 false)
                                 1 TIMER ready-to-serve-state))
   (make-world (list (make-ball 330 384 0 0))
               (make-racket 330 384 0 0 0 0 false) 1 3 "ready-to-serve-state")
   "world-after-tick when in ready to serve state")

  (check-equal?
   (world-after-tick (make-world (list (make-ball 80 80 3 9))
                                 (make-racket 100 100 0 3 0 0 false)
                                 1 TIMER rally-state))
   (make-world (list (make-ball 83 89 3 9))
               (make-racket 100 103 0 3 0 0 false) 1 3 "rally-state")
   "world-after-tick when in rally state")
  
  (check-equal?
   (world-after-tick (make-world (list (make-ball 80 450 3 9))
                                 (make-racket 100 100 0 3 0 0 false)
                                 1 2 paused-state))
   (make-world (list (make-ball 80 450 3 9))
               (make-racket 100 100 0 3 0 0 false) 1 1 "paused-state")
   "world-after-tick when in paused state")

  (check-equal?
   (world-after-tick (make-world (list (make-ball 80 450 3 9)
                                       (make-ball 20 250 3 2))
                                 (make-racket 100 100 0 3 0 0 false)
                                 1 1 paused-state))
   (make-world (list (make-ball 330 384 0 0))
               (make-racket 330 384 0 0 0 0 false) 1 3 "ready-to-serve-state")
   "world-after-tick when in ready to serve state"))

;; help function for checking if the world is in paused state
;; world-in-paused-state? : World -> Boolean
;; GIVEN: a world
;; RETURNS: true if the world is in paused-state 
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of World

(define (world-in-paused-state? w)
  (string=? (world-state w) "paused-state"))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-in-paused-state? (make-world (list (make-ball 30 84 3 6))
                                       (make-racket 40 38 2 5 0 0 false)
                                       1 TIMER paused-state))
   #true
   "should be true"))

;; help function to check if world is in rally state
;; world-in-rally-state? : World -> Boolean
;; GIVEN: a world
;; RETURNS: true iff the world is in its rally state
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of World

(define (world-in-rally-state? w)
  (string=? (world-state w) rally-state))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-in-rally-state? (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state))
   #true
   "should be true")

  (check-equal?
   (world-in-rally-state? (make-world (list (make-ball 30 84 0 0))
                                      (make-racket 40 38 0 0 0 0 false)
                                      1 TIMER paused-state))
   #false
   "should be false")

  (check-equal?
   (world-in-rally-state? (make-world (list)
                                      (make-racket 40 38 0 0 0 0 false)
                                      1 TIMER paused-state))
   #false
   "should be false"))

;; world-after-updating-blist : World -> World
;; GIVEN: any world with many balls in simulation
;; RETURNS: the world that should be after removing balls from list
;;          which have left the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World
;;                  and HOF filter on BallList

(define (world-after-updating-blist w)
  (make-world (filter
               ;; Ball -> Boolean
               ;; RETURNS: false if the ball collides with back wall
               (lambda (b) (not (ball-will-collide-back-wall? b)))
               (world-balls w))
              (world-racket w)
              (world-speed w)
              TIMER
              rally-state))
#;
(define (world-after-updating-blist w)
  (make-world (filter-balls (world-balls w))
              (world-racket w)
              (world-speed w)
              TIMER
              rally-state))

;; filter-balls : BallList -> BallList
;; GIVEN: a list of balls
;; RETURNS: a list of balls after removing the balls that collide the back wall
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the HOF filter on BallList
#;
(define (filter-balls blist)
  (filter
   ;; Ball -> Boolean
   ;; RETURNS: false if the ball collides with back wall
   (lambda (b) (not (ball-will-collide-back-wall? b))) blist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-in-rally : World -> World
;; GIVEN:   any world that is in simulation
;; RETURNS: the world that should be during that point of simulation
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World
;;                  and HOF andmap on blist

(define (world-in-rally w)
  (cond
    [(or (empty? (world-balls w))
         (andmap ball-will-collide-back-wall? (world-balls w))
         (racket-will-collide-back-wall? (world-racket w))
         (racket-will-collide-front-wall? (world-racket w)))
     (world-paused-to-reset w)]
    [else (if (racket-selected? (world-racket w))
              (racket-when-selected w)
              (racket-when-not-selected w))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-in-rally (make-world (list (make-ball 300 646 2 5))
                               (make-racket 40 38 2 5 0 0 false)
                               1 TIMER rally-state))
   (make-world (list (make-ball 300 646 2 5))
               (make-racket 40 38 0 0 0 0 false)
               1 TIMER paused-state)
   "racket will collide back wall")
  (check-equal?
   (world-in-rally (make-world (list (make-ball 300 646 2 5))
                               (make-racket 40 38 2 5 0 0 true)
                               1 TIMER rally-state))
   (make-world (list (make-ball 300 646 2 5))
               (make-racket 40 38 0 0 0 0 false)
               1 TIMER paused-state)
   "racket is selected")
  (check-equal?
   (world-in-rally (make-world (list (make-ball 300 236 2 5))
                               (make-racket 40 38 2 5 0 0 true)
                               1 TIMER rally-state))
   (make-world (list (make-ball 302 241 2 5))
               (make-racket 40 38 2 5 0 0 true)
               1 TIMER rally-state)
   "racket is not selected"))

;; balls-will-collide-back-wall? : BallList -> Boolean
;; GIVEN: a list of balls
;; RETURNS: true iff the balls collide the back wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the HOF andmap on BallList
#;
(define (balls-will-collide-back-wall? blist)
  (andmap ball-will-collide-back-wall? blist))
#;
(define (balls-will-collide-back-wall? blist)
  (andmap
   ;; Ball -> Boolean
   ;; RETURNS: true if the ball collides with back wall
   (lambda (b) (and (< (ball-y b) BACK-WALL-Y)
                    (>= (+ (ball-y b) (ball-vy b)) BACK-WALL-Y)))
   blist))
#;
(define (balls-will-collide-back-wall? blist)
  (cond
    [(empty? (rest blist)) (ball-will-collide-back-wall? (first blist))]
    [else (and (ball-will-collide-back-wall? (first blist))
               (balls-will-collide-back-wall? (rest blist)))]))

;; TESTS:
#;
(begin-for-test
  (check-equal?
   (balls-will-collide-back-wall? (list (make-ball 80 80 3 6)))
   #false
   "balls must not collide back wall")

  (check-equal?
   (balls-will-collide-back-wall?
    (list (make-ball 300 646 2 5) (make-ball 300 644 2 5)))
   #true
   "balls must collide back wall"))

;; ball-will-collide-back-wall? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true iff the ball collides the back wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-will-collide-back-wall? b)
  (and (< (ball-y b) BACK-WALL-Y)
       (>= (+ (ball-y b) (ball-vy b))
           BACK-WALL-Y)))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-will-collide-back-wall? (make-ball 80 80 3 6))
   #false
   "ball must not collide back wall")

  (check-equal?
   (ball-will-collide-back-wall?
    (make-ball 300 644 2 5))
   #true
   "ball must collide back wall"))

;; racket-will-collide-back-wall? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the racket collides the back wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Racket

(define (racket-will-collide-back-wall? r)
  (>= (+ (racket-y r) (racket-vy r)) BACK-WALL-Y))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-will-collide-back-wall? (make-racket 80 80 3 6 0 0 false))
   #false
   "racket must not collide back wall")

  (check-equal?
   (racket-will-collide-back-wall? (make-racket 300 648 2 5 0 0 false))
   #true
   "racket must collide back wall"))

;; racket-will-collide-front-wall? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the racket is collides the front wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Racket

(define (racket-will-collide-front-wall? r)
  (<= (+ (racket-y r) (racket-vy r)) FRONT-WALL-Y))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-will-collide-front-wall? (make-racket 80 80 3 6 0 0 false))
   #false
   "racket must not collide front wall")

  (check-equal?
   (racket-will-collide-front-wall? (make-racket 300 4 2 -5 0 0 false))
   #true
   "racket must collide front wall"))

;; racket-when-selected : World -> World
;; GIVEN:   any world that is in simulation
;; RETURNS: the world that should be during that point of simulation
;;          when racket is selected
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World

(define (racket-when-selected w)
  (make-world (balls-after-tick (world-balls w) (world-racket w))
              (world-racket w)
              (world-speed w)
              TIMER
              rally-state))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-when-selected (make-world (list (make-ball 300 236 2 5))
                                     (make-racket 40 38 2 5 0 0 true)
                                     1 TIMER rally-state))
   (make-world (list (make-ball 302 241 2 5))
               (make-racket 40 38 2 5 0 0 true)
               1 TIMER rally-state)
   "racket should be selected"))

;; racket-when-not-selected : World -> World
;; GIVEN:   any world that is in simulation
;; RETURNS: the world that should be during that point of simulation
;;          when racket is not selected
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World

(define (racket-when-not-selected w)
  (make-world (balls-after-tick (world-balls w) (world-racket w))
              (racket-after-tick (world-racket w) (world-balls w))
              (world-speed w)
              TIMER
              rally-state))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-when-not-selected (make-world (list (make-ball 300 646 2 5))
                                         (make-racket 40 38 2 5 0 0 false)
                                         1 TIMER rally-state))
   (make-world (list (make-ball 302 651 2 5))
               (make-racket 42 43 2 5 0 0 false)
               1 TIMER rally-state)
   "racket should not be selected"))

;; help function for checking balls after tick
;; balls-after-tick : BallList Racket -> BallList
;; GIVEN: a list of balls and a racket
;; RETURNS: the balls that should follow the given balls after a tick
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the HOF map on BallList

(define (balls-after-tick blist r)
  (map
   ;; BallList -> Ball
   ;; RETURNS: the ball that should follow the given ball after a tick
   (lambda (n) (ball-after-tick n r)) blist))

#;
(define (balls-after-tick blist r)
  (cond
    [(empty? (rest blist)) (cons (ball-after-tick (first blist) r) empty)]
    [else (cons (ball-after-tick (first blist) r)
                (balls-after-tick (rest blist) r))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (balls-after-tick (list (make-ball 120 380 0 0))
                     (make-racket 100 100 0 0 0 0 false))
   (list (make-ball 120 380 0 0))
   "balls-after-tick")

  (check-equal?
   (balls-after-tick (list (make-ball 120 380 3 9))
                     (make-racket 100 100 3 0 0 0 false))
   (list (make-ball 123 389 3 9))
   "balls-after-tick")

  (check-equal?
   (balls-after-tick (list (make-ball 198 295 5 9))
                     (make-racket 200 300 3 0 0 0 false))
   (list (make-ball 203 286 5 -9))
   "balls-after-tick will collide with racket")

  (check-equal?
   (balls-after-tick (list (make-ball 20 0 3 9) (make-ball 230 220 2 4))
                     (make-racket 12 38 3 0 0 0 false))
   (list (make-ball 23 9 3 9) (make-ball 232 224 2 4))
   "balls-after-tick will collide with front wall"))

;; help function for checking balls after tick
;; ball-after-tick : Ball Racket -> Ball
;; GIVEN: a ball and a racket
;; RETURNS: the ball that should follow the given ball after a tick
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Cases on behavior of Ball

(define (ball-after-tick b r)
  (cond
    [(ball-will-collide-racket? b r)
     (ball-after-racket-collision b r)]
    [(ball-will-collide-wall? b) (ball-after-wall-collision b)]
    [else (ball-no-collision b)]))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-tick (make-ball 120 2 3 -3) (make-racket 120 100 5 -2 0 0 false))
   (make-ball 123 1 3 3)
   "ball after tick"))

;; help function for checking if balls will collide with racket
;; balls-will-collide-racket? : BallList Racket -> Boolean
;; GIVEN: a list of balls and a racket
;; RETURNS: true if the balls will collide to the racket in the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the HOF ormap on BallList

(define (balls-will-collide-racket? blist r)
  (ormap
   ;; Ball -> Boolean
   ;; RETURNS: true iff the ball will collide to the racket
   (lambda (n) (ball-will-collide-racket? n r)) blist))

#;
(define (balls-will-collide-racket? blist r)
  (cond 
    [(empty? (rest blist)) (ball-will-collide-racket? (first blist) r) ]
    [else (or (ball-will-collide-racket? (first blist) r)
              (balls-will-collide-racket? (rest blist) r))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (balls-will-collide-racket? (list (make-ball 119 98 3 5)
                                     (make-ball 220 110 5 2))
                               (make-racket 120 100 5 -2 0 0 false))
   false
   "balls will not collide racket"))

;; help function for checking if ball will collide with racket
;; ball-will-collide-racket? : Ball Racket -> Boolean
;; GIVEN: a ball and a racket
;; RETURNS: true if the ball will collide to the racket in the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-will-collide-racket? b r)
  (and (is-ball-crossing-racket-y? b r)
       (>= (ball-vy b) 0)
       (< (- (racket-x r) RACKET-HALF-WIDTH)
          (calculate-ball-racket-collide-point b r))
       (> (+ (racket-x r) RACKET-HALF-WIDTH)
          (calculate-ball-racket-collide-point b r))))

;; is-ball-crossing-racket-y? : Ball Racket -> Boolean
;; GIVEN: a ball and a racket
;; RETURNS: true if the ball will cross the racket
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (is-ball-crossing-racket-y? b r)
  (and
   (< (ball-y b) (+ (racket-y r) (racket-vy r)))
   (>= (+ (ball-y b) (ball-vy b)) (+ (racket-y r) (racket-vy r)))))

;; help function for checking ball's slope
;; calculate-slope : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: an integer which depicts the slope of the ball's route
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (calculate-slope b)
  (/ (- (ball-y b)
        (- (ball-y b) (ball-vy b)))
     (- (ball-x b)
        (- (ball-x b) (ball-vx b)))))

;; TESTS:
(begin-for-test
  (check-equal?
   (calculate-slope (make-ball 120 100 3 9))
   3
   "calculate-slope"))

;; help function for checking ball's slope
;; calculate-ball-racket-collide-point : Ball Racket -> Integer
;; GIVEN: a ball
;; RETURNS: an integer at which ball will collide with racket
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball 

(define (calculate-ball-racket-collide-point b r)
  (+ (- (ball-x b) (ball-vx b))
     (/ (- (racket-y r)
           (- (ball-y b) (ball-vy b))))))

;; help function for checking if ball will collide with wall
;; ball-will-collide-wall? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true if the ball will collide the wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-will-collide-wall? b)
  (or (<= (+ (ball-y b) (ball-vy b)) FRONT-WALL-Y)
      (<= (+ (ball-x b) (ball-vx b)) LEFT-WALL-X)
      (>= (+ (ball-x b) (ball-vx b)) RIGHT-WALL-X)))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-will-collide-wall? (make-ball 120 2 3 -3))
   #true
   "ball collides with wall"))

;; help function for ball that will not collide
;; ball-no-collision : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: the ball that should follow the given ball after not colliding
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Ball

(define (ball-no-collision b)
  (make-ball (+ (ball-x b) (ball-vx b))
             (+ (ball-y b) (ball-vy b))
             (ball-vx b)
             (ball-vy b)))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-no-collision (make-ball 120 120 3 9))
   (make-ball 123 129 3 9)
   "ball after no collision"))

;; help function for balls that will collide with the racket
;; ball-after-racket-collision : Ball Racket -> Ball
;; GIVEN: a ball and a racket
;; RETURNS: the ball that should follow the given ball
;;          after colliding with the racket
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Ball

(define (ball-after-racket-collision b r)
  (make-ball (+ (ball-x b) (ball-vx b))
             (+ (ball-y b) (- (racket-vy r) (ball-vy b)))
             (ball-vx b)
             (- (racket-vy r) (ball-vy b))))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-racket-collision (make-ball 120 120 2 5)
                                (make-racket 120 120 5 7 0 0 false))
   (make-ball 122 122 2 2)
   "ball after colliding racket"))

;; help function for ball that will collide with the walls of the court
;; ball-after-wall-collision : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: the ball that should follow the given ball
;;          after colliding with the walls
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Cases on ball's collision behavior

(define (ball-after-wall-collision b)
  (cond [(ball-will-collide-front-wall? b)
         (ball-after-colliding-front-wall b)]
        [(ball-will-collide-left-wall? b)
         (ball-after-colliding-left-wall b)]
        [(ball-will-collide-right-wall? b)
         (ball-after-colliding-right-wall b)]))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-wall-collision (make-ball 120 2 3 -3))
   (make-ball 123 1 3 3)
   "ball after colliding front wall")
  (check-equal?
   (ball-after-wall-collision (make-ball 3 130 -3 5))
   (make-ball 0 135 3 5)
   "ball after colliding left wall")
  (check-equal?
   (ball-after-wall-collision (make-ball 424 30 3 -2))
   (make-ball 423 28 -3 -2)
   "ball after colliding right wall"))

;; help function for checking if ball will collide with the front wall
;; ball-will-collide-front-wall? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true if the ball will collide with the front wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-will-collide-front-wall? b)
  (<= (+ (ball-y b) (ball-vy b)) FRONT-WALL-Y))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-will-collide-front-wall? (make-ball 120 2 3 -3))
   #true
   "ball after colliding front wall"))

;; ball-after-colliding-front-wall : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: the ball that should follow the given ball
;;          after colliding with the front wall
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-after-colliding-front-wall b)
  (make-ball (+ (ball-x b) (ball-vx b))
             (- (+ (ball-y b) (ball-vy b)))
             (ball-vx b)
             (- (ball-vy b))))

;; help function for checking if ball will collide with the left wall
;; ball-will-collide-left-wall? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true if the ball will collide with the left wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-will-collide-left-wall? b)
  (<= (+ (ball-x b) (ball-vx b)) LEFT-WALL-X))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-will-collide-left-wall? (make-ball 3 130 -3 5))
   #true
   "ball after colliding left wall"))

;; ball-after-colliding-left-wall : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: the ball that should follow the given ball
;;          after colliding with the front wall
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Ball

(define (ball-after-colliding-left-wall b)
  (make-ball (- (+ (ball-x b) (ball-vx b)))
             (+ (ball-y b) (ball-vy b))
             (- (ball-vx b))
             (ball-vy b)))

;; help function for checking if ball will collide with the right wall
;; ball-will-collide-right-wall? : Ball -> Boolean
;; GIVEN: a ball
;; RETURNS: true if the ball will collide with the right wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Ball

(define (ball-will-collide-right-wall? b)
  (>= (+ (ball-x b) (ball-vx b)) RIGHT-WALL-X))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-will-collide-right-wall? (make-ball 424 30 3 -2))
   #true
   "ball after colliding right wall"))

;; ball-after-colliding-right-wall : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: the ball that should follow the given ball
;;          after colliding with the front wall
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Ball

(define (ball-after-colliding-right-wall b)
  (make-ball (- RIGHT-WALL-X (- (+ (ball-x b) (ball-vx b)) RIGHT-WALL-X))
             (+ (ball-y b) (ball-vy b))
             (- (ball-vx b))
             (ball-vy b)))
  
;; help function for checking racket after tick
;; racket-after-tick : Racket BallList -> Racket
;; GIVEN: a racket and a list of balls
;; RETURNS: the racket that should follow the given racket after a tick
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Cases on racket's collision behavior

(define (racket-after-tick r blist)
  (cond
    [(balls-will-collide-racket? blist r) (racket-after-ball-collision r)]
    [(racket-will-collide-left-wall? r) (racket-after-left-wall-collision r)]
    [(racket-will-collide-right-wall? r) (racket-after-right-wall-collision r)]
    [else (racket-no-collision r)]))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-tick (make-racket 300 300 2 5 0 0 false)
                      (list (make-ball 80 80 3 9)))
   (make-racket 302 305 2 5 0 0 false)
   "racket-after-tick")

  (check-equal?
   (racket-after-tick (make-racket 200 300 3 0 0 0 false)
                      (list (make-ball 198 295 5 9)))
   (make-racket 200 300 3 0 0 0 false)
   "racket-after-tick")

  (check-equal?
   (racket-after-tick (make-racket 0 300 2 5 0 0 false)
                      (list (make-ball 240 343 3 9)))
   (make-racket 23 300 0 5 0 0 false)
   "racket-after-tick")

  (check-equal?
   (racket-after-tick (make-racket 410 340 2 5 0 0 false)
                      (list (make-ball 240 343 3 9)))
   (make-racket 402 340 0 5 0 0 false)
   "racket-after colliding right wall"))

;; help function for racket that will collide with the balls
;; racket-after-ball-collision : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after colliding with the balls
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-after-ball-collision r)
  (if (< (racket-vy r) 0)
      (make-racket (racket-x r) (racket-y r)
                   (racket-vx r) 0
                   (racket-mx r) (racket-my r)
                   (racket-selected? r))
      (make-racket (racket-x r) (racket-y r)
                   (racket-vx r) (racket-vy r)
                   (racket-mx r) (racket-my r)
                   (racket-selected? r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-ball-collision (make-racket 233 240 2 -5 0 0 true))
   (make-racket 233 240 2 0 0 0 true)
   "racket-after-ball-collision")
  (check-equal?
   (racket-after-ball-collision (make-racket 233 240 2 5 0 0 true))
   (make-racket 233 240 2 5 0 0 true)
   "racket after no ball collision"))

;; help function for checking if racket will collide with the left wall
;; racket-will-collide-left-wall? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true if the racket will collide with the left wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Racket

(define (racket-will-collide-left-wall? r)
  (< (- (racket-x r) (floor RACKET-HALF-WIDTH)) LEFT-WALL-X))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-will-collide-left-wall? (make-racket 330 240 2 5 0 0 true))
   #false
   "racket will not collide")

  (check-equal?
   (racket-will-collide-left-wall? (make-racket 0 240 2 5 0 0 true))
   #true
   "racket will collide"))

;; help function for checking if racket will collide with the right wall
;; racket-will-collide-right-wall? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true if the racket will collide with the right wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Racket

(define (racket-will-collide-right-wall? r)
  (> (+ (racket-x r) RACKET-HALF-WIDTH) RIGHT-WALL-X))

;; help function for racket that will collide with the left wall of the court
;; racket-after-left-wall-collision : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after colliding with the left wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-after-left-wall-collision r)
  (make-racket RACKET-HALF-WIDTH (racket-y r) 0 (racket-vy r)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-left-wall-collision (make-racket 20 240 2 5 0 0 false))
   (make-racket 23 240 0 5 0 0 false)
   "racket must collide left wall"))

;; help function for racket that will collide with the right wall of the court
;; racket-after-right-wall-collision : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after colliding with the right wall of the court
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-after-right-wall-collision r)
  (make-racket (- RIGHT-WALL-X RACKET-HALF-WIDTH) (racket-y r) 0 (racket-vy r)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-right-wall-collision (make-racket 326 100 2 5 0 0 false))
   (make-racket 402 100 0 5 0 0 false)
   "racket must collide right wall"))

;; help function for racket that will not collide
;; racket-no-collision : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket after not colliding
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-no-collision r)
  (make-racket (+ (racket-x r) (racket-vx r)) (+ (racket-y r)(racket-vy r))
               (racket-vx r) (racket-vy r)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-no-collision (make-racket 233 323 2 5 0 0 false))
   (make-racket 235 328 2 5 0 0 false)
   "racket must not collide"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world and a key event
;; RETURNS: the world that should follow the given world
;;          after the given key event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Cases on the state of World

(define (world-after-key-event w kev)
  (cond [(world-ready-to-serve? w)
         (if (is-pause-key-event? kev)
             (world-ready-to-rally w)
             (world-after-tick w))]
        [(world-in-rally-state? w)
         (cond [(is-pause-key-event? kev) (world-paused-to-reset w)]
               [(new-ball-key-event? kev) (world-with-new-balls w)]
               [(or (left-arrow-key-event? kev)
                    (right-arrow-key-event? kev)
                    (up-arrow-key-event? kev)
                    (down-arrow-key-event? kev))
                (world-after-arrow-event w kev)]
               [else (world-after-tick w)])]
        [(world-in-paused-state? w) w]))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event (make-world (list (make-ball 330 384 0 0))
                                      (make-racket 330 384 0 0 0 0 false)
                                      1 TIMER ready-to-serve-state)  " ")
   (make-world (list (make-ball 330 384 3 -9))
               (make-racket 330 384 0 0 0 0 false) 1 3 "rally-state")
   "spacebar key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 330 384 0 0))
                                      (make-racket 330 384 0 0 0 0 false)
                                      1 TIMER ready-to-serve-state)  "left")
   (make-world (list (make-ball 330 384 0 0))
               (make-racket 330 384 0 0 0 0 false) 1 3 "ready-to-serve-state")
   "left key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  " ")
   (make-world (list (make-ball 30 84 3 6))
               (make-racket 40 38 0 0 0 0 false) 1 3 "paused-state")
   "spacebar key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  "left")
   (make-world (list (make-ball 30 84 3 6))
               (make-racket 40 38 1 5 0 0 false) 1 3 "rally-state")
   "left key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  "right")
   (make-world (list (make-ball 30 84 3 6))
               (make-racket 40 38 3 5 0 0 false) 1 3 "rally-state")
   "right key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  "up")
   (make-world (list (make-ball 30 84 3 6))
               (make-racket 40 38 2 4 0 0 false) 1 3 "rally-state")
   "up key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  "down")
   (make-world (list (make-ball 30 84 3 6))
               (make-racket 40 38 2 6 0 0 false) 1 3 "rally-state")
   "down key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  "b")
   (make-world (list (make-ball 30 84 3 6) (make-ball 330 384 3 -9))
               (make-racket 40 38 2 5 0 0 false) 1 3 "rally-state")
   "b key")

  (check-equal?
   (world-after-key-event (make-world (list (make-ball 30 84 3 6))
                                      (make-racket 40 38 2 5 0 0 false)
                                      1 TIMER rally-state)  "w")
   (make-world (list (make-ball 33 90 3 6))
               (make-racket 42 43 2 5 0 0 false) 1 3 "rally-state")
   "any key")
 
  (check-equal?
   (world-after-key-event (make-world (list (make-ball 80 450 0 0))
                                      (make-racket 100 100 0 0 0 0 false)
                                      1 3 paused-state) " ")
   (make-world (list (make-ball 80 450 0 0))
               (make-racket 100 100 0 0 0 0 false) 1 3 "paused-state")
   "world-after-key-event"))

;; world-ready-to-rally : World -> World
;; GIVEN: any world that is ready to simulate
;; RETURNS: the world at the start of simulation
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World and Racket

(define (world-ready-to-rally w)
  (make-world (list (make-ball BALL-IPX BALL-IPY BALL-RVX BALL-RVY))
              (make-racket RACKET-IPX RACKET-IPY
                           RACKET-IVX RACKET-IVY
                           (racket-mx (world-racket w))
                           (racket-my (world-racket w)) false)
              (world-speed w)
              TIMER
              rally-state))

;; help function for world that is paused to reset
;; world-paused-to-reset : World -> World
;; GIVEN: a world
;; RETURNS: the world that should follow the given world
;;          after a pause button is pressed
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor of World and Racket

(define (world-paused-to-reset w)
  (make-world (world-balls w)
              (make-racket (racket-x (world-racket w))
                           (racket-y (world-racket w))
                           RACKET-IVX RACKET-IVX
                           (racket-mx (world-racket w))
                           (racket-my (world-racket w))
                           false)               
              (world-speed w)
              (compute-timer w)
              paused-state))

;; compute-timer : World -> Real
;; GIVEN: a world
;; RETURNS: a real number that gives speed after converting it into ticks
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of World

(define (compute-timer w)
  (/ 3 (world-speed w))) 

;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction

(define (is-pause-key-event? ke)
  (key=? ke " "))

;; TESTS:
(begin-for-test
  (check-equal?
   (is-pause-key-event? " ")
   #true
   "is-pause-key-event?")

  (check-equal?
   (is-pause-key-event? "left")
   #false
   "is-pause-key-event?"))

;; help function for key event
;; new-ball-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a new ball instruction

(define (new-ball-key-event? ke)
  (key=? ke "b"))

;; TESTS:
(begin-for-test
  (check-equal?
   (new-ball-key-event? "b")
   #true
   "new-ball-key-event")

  (check-equal?
   (new-ball-key-event? "left")
   #false
   "should be a new-ball-key-event"))

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")
(define new-ball-key-event "b")

;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;; world-with-new-balls : World -> World
;; GIVEN: a world with existing balls
;; RETURNS: the world with existing and new balls
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World
(define (world-with-new-balls w)
  (make-world (append (world-balls w)
                      (list (make-ball BALL-IPX BALL-IPY BALL-RVX BALL-RVY)))
              (world-racket w)
              (world-speed w)
              TIMER
              (world-state w)))

;; world-after-arrow-event : World ArrowEvent -> World
;; GIVEN: a World and an arrow event
;; RETURNS: the World that should follow the given World
;;          after the given arrow event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World

(define (world-after-arrow-event w kev)
  (make-world (world-balls w)
              (racket-after-arrow-event (world-racket w) kev)
              (world-speed w)
              TIMER
              rally-state))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-arrow-event (make-world (list (make-ball 320 284 3 9))
                                        (make-racket 140 318 5 2 0 0 false)
                                        1 TIMER rally-state) "left")
   (make-world (list (make-ball 320 284 3 9))
               (make-racket 140 318 4 2 0 0 false) 1 3 "rally-state")
   "world-after left arrow-event")

  (check-equal?
   (world-after-arrow-event (make-world (list (make-ball 320 284 3 9))
                                        (make-racket 140 318 5 2 0 0 false)
                                        1 TIMER rally-state) "up")
   (make-world (list (make-ball 320 284 3 9))
               (make-racket 140 318 5 1 0 0 false) 1 3 "rally-state")
   "world-after up arrow-event")

  (check-equal?
   (world-after-arrow-event (make-world (list (make-ball 320 284 3 9))
                                        (make-racket 140 318 5 2 0 0 false)
                                        1 TIMER rally-state) "down")
   (make-world (list (make-ball 320 284 3 9))
               (make-racket 140 318 5 3 0 0 false) 1 3 "rally-state")
   "world-after down arrow-event")

  (check-equal?
   (world-after-arrow-event (make-world (list (make-ball 320 284 3 9))
                                        (make-racket 140 318 5 2 0 0 false)
                                        1 TIMER rally-state) "right")
   (make-world (list (make-ball 320 284 3 9))
               (make-racket 140 318 6 2 0 0 false) 1 3 "rally-state")
   "world-after right arrow-event"))

;; racket-after-arrow-event : Racket ArrowEvent -> Racket
;; GIVEN: a racket and an arrow event
;; RETURNS: the racket that should follow the given racket
;;          after the given arrow event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Cases on arrow key event

(define (racket-after-arrow-event r aev)
  (cond [(left-arrow-key-event? aev)
         (racket-moving-left r)]
        [(right-arrow-key-event? aev)
         (racket-moving-right r)]
        [(up-arrow-key-event? aev)
         (racket-moving-up r)]
        [(down-arrow-key-event? aev)
         (racket-moving-down r)]))

;; help function for arrow key event
;; left-arrow-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents left arrow instruction

(define (left-arrow-key-event? ae)
  (key=? ae "left"))

;; help function for arrow key event
;; right-arrow-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents right arrow instruction

(define (right-arrow-key-event? ae)
  (key=? ae "right"))

;; help function for arrow key event
;; up-arrow-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents up arrow instruction

(define (up-arrow-key-event? ae)
  (key=? ae "up"))

;; help function for arrow key event
;; down-arrow-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents down arrow instruction

(define (down-arrow-key-event? ae)
  (key=? ae "down"))

;; help function for racket moving in left direction
;; racket-moving-left : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after the given left arrow event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-moving-left r)
  (make-racket (racket-x r) (racket-y r)
               (- (racket-vx r) 1) (racket-vy r)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-moving-left (make-racket 30 318 5 2 0 0 false))
   (make-racket 30 318 4 2 0 0 false)
   "racket after moving-left")
  (check-equal?
   (racket-moving-left (make-racket 140 318 5 2 0 0 false))
   (make-racket 140 318 4 2 0 0 false)
   "racket after moving-left"))

;; help function for racket moving in right direction
;; racket-moving-right : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after the given right arrow event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-moving-right r) 
  (make-racket (racket-x r) (racket-y r)
               (+ (racket-vx r) 1) (racket-vy r)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-moving-right (make-racket 340 318 5 2 0 0 false))
   (make-racket 340 318 6 2 0 0 false)
   "racket should be moved right")
  (check-equal?
   (racket-moving-right (make-racket 140 318 5 2 0 0 false))
   (make-racket 140 318 6 2 0 0 false)
   "racket should be moved right"))

;; help function for racket moving in up direction
;; racket-moving-up : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after the given up arrow event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-moving-up r)
  (make-racket (racket-x r) (racket-y r)
               (racket-vx r) (- (racket-vy r) 1)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-moving-up (make-racket 140 318 5 2 0 0 false))
   (make-racket 140 318 5 1 0 0 false)
   "racket should be moved up"))

;; help function for racket moving in down direction
;; racket-moving-down : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the given racket
;;          after the given down arrow event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket

(define (racket-moving-down r)
  (make-racket (racket-x r) (racket-y r)
               (racket-vx r) (+ (racket-vy r) 1)
               (racket-mx r) (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-moving-down (make-racket 140 318 5 2 0 0 false))
   (make-racket 140 318 5 3 0 0 false)
   "racket should be moved down"))

;; world-balls : World -> BallList
;; GIVEN: a world
;; RETURNS: a list of balls that are present in the world
;;         (but does not include any balls that have disappeared
;;          by colliding with the back wall)
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World,
;;                  BallList and Racket
;; TESTS:
(begin-for-test
  (check-equal?
   (world-balls (make-world (list (make-ball 80 80 0 0))
                            (make-racket 100 100 0 0 0 0 false)
                            1 TIMER rally-state))
   (list (make-ball 80 80 0 0))))

;; world-racket : World -> Racket
;; GIVEN: a world
;; RETURNS: the racket that's present in the world
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World,
;;                  BallList and Racket 
;; TESTS:
(begin-for-test 
  (check-equal?
   (world-racket (make-world (list (make-ball 80 80 0 0))
                             (make-racket 100 100 0 0 0 0 false)
                             1 TIMER rally-state))
   (make-racket 100 100 0 0 0 0 false)))

;; ball-x : Ball -> Integer
;; ball-y : Ball -> Integer
;; racket-x : Racket -> Integer
;; racket-y : Racket -> Integer
;; GIVEN: a racket or ball
;; RETURNS: the x or y coordinate of that item's position,
;;          in graphics coordinates
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket or BallList
;; TESTS:
(begin-for-test
  (check-equal?
   (ball-x (first (list (make-ball 200 300 3 9)))) 200)
  (check-equal?
   (ball-y (first (list (make-ball 200 300 3 9)))) 300)
  (check-equal?
   (racket-x (make-racket 150 250 2 5 0 0 false)) 150)
  (check-equal?
   (racket-y (make-racket 150 250 2 5 0 0 false)) 250))

;; ball-vx : Ball -> Integer
;; ball-vy : Ball -> Integer
;; racket-vx : Racket -> Integer
;; racket-vy : Racket -> Integer
;; GIVEN: a racket or ball
;; RETURNS: the vx or vy component of that item's velocity,
;;          in pixels per tick
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of Racket or BallList
;; TESTS:
(begin-for-test
  (check-equal?
   (ball-vx (first (list (make-ball 200 300 3 9)))) 3)
  (check-equal?
   (ball-vy (first (list (make-ball 200 300 3 9)))) 9)
  (check-equal?
   (racket-vx (make-racket 150 250 2 5 0 0 false)) 2)
  (check-equal?
   (racket-vy (make-racket 150 250 2 5 0 0 false)) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Int Int MouseEvent -> World
;; GIVEN: a world, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the world that should follow the given world after
;;          the given mouse event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the constructor template of World and Racket

(define (world-after-mouse-event w mx my mev)
  (if (string=? (world-state w) rally-state)
      (make-world (world-balls w)
                  (racket-after-mouse-event (world-racket w) mx my mev)
                  (world-speed w)
                  TIMER
                  rally-state)
      w))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event (make-world (list (make-ball 30 240 3 9))
                                        (make-racket 330 240 2 5 0 0 true)
                                        1 TIMER rally-state) 330 245 "drag")
   (make-world
    (list (make-ball 30 240 3 9))
    (make-racket 330 245 2 5 0 0 true) 1 3 "rally-state")
   "world-after-mouse-event drag")

  (check-equal?
   (world-after-mouse-event (make-world (list (make-ball 30 240 3 9))
                                        (make-racket 330 240 2 5 0 0 false)
                                        1 TIMER paused-state) 330 245 "move")
   (make-world
    (list (make-ball 30 240 3 9))
    (make-racket 330 240 2 5 0 0 false) 1 3 "paused-state")
   "world-after no specific mouse-event"))

;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the racket as it should be after the given mouse event
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Cases on mouse event mev

(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev button-down-event) (racket-after-button-down r mx my)]
    [(mouse=? mev drag-event) (racket-after-drag r mx my)]
    [(mouse=? mev button-up-event) (racket-after-button-up r mx my)]
    [else r]))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-mouse-event (make-racket 330 245 0 0 0 0 true)
                             335 240 "button-down")
   (make-racket 330 245 0 0 5 -5 true)
   "racket-after button down mouse-event")

  (check-equal?
   (racket-after-mouse-event (make-racket 330 245 0 0 0 0 true)
                             335 240 "button-up")
   (make-racket 330 245 0 0 0 0 false)
   "racket-after button up mouse-event")

  (check-equal?
   (racket-after-mouse-event (make-racket 330 245 0 0 0 0 false)
                             335 240 "move")
   (make-racket 330 245 0 0 0 0 false)
   "racket-after no specific mouse-event"))

;; helper functions:

;; racket-after-button-down : Racket Integer Integer -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event
;; RETURNS: the racket following a button-down at the given location
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using templates of Racket on r

(define (racket-after-button-down r mx my)
  (if (in-racket? r mx my)
      (make-racket (racket-x r) (racket-y r)
                   (racket-vx r) (racket-vy r)
                   (- mx (racket-x r)) (- my (racket-y r)) true)
      r))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-button-down (make-racket 330 245 2 4 0 0 false) 30 45)
   (make-racket 330 245 2 4 0 0 false)
   "racket must get selected"))

;; racket-after-drag : Racket Integer Integer -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event
;; RETURNS: the racket following a drag at the given location
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using templates of Racket on r

(define (racket-after-drag r mx my)
  (if (racket-selected? r)
      (make-racket (- mx (racket-mx r))  (- my (racket-my r))
                   (racket-vx r) (racket-vy r)
                   (racket-mx r) (racket-my r)         
                   true)
      r))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-drag (make-racket 330 245 2 4 0 0 false) 330 245)
   (make-racket 330 245 2 4 0 0 false)
   "racket must start to drag"))

;; racket-after-button-up : Racket Integer Integer -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event
;; RETURNS: the racket following a button-up at the given location
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using templates of Racket on r

(define (racket-after-button-up r x y)
  (if (racket-selected? r)
      (make-racket (racket-x r) (racket-y r) (racket-vx r) (racket-vy r)
                   0 0 false)
      r))

;; TESTS:
(begin-for-test
  (check-equal?
   (racket-after-button-up (make-racket 330 245 0 0 0 0 false) 330 245)
   (make-racket 330 245 0 0 0 0 false)
   "racket must get deselected"))

;; in-racket? : Racket Integer Integer -> Boolean
;; GIVEN: a racket, the x and y coordinates of a mouse event
;; RETURNS: true iff the given coordinate is inside the bounding box of
;;          the given racket.
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using observer template of Racket on r

(define (in-racket? r x y)
  (and
   (<= (- (racket-x r) POINTER-DISTANCE)
       x
       (+ (racket-x r) POINTER-DISTANCE))
   (<= (- (racket-y r) POINTER-DISTANCE)
       y
       (+ (racket-y r) POINTER-DISTANCE))))

;; TESTS:
(begin-for-test
  (check-equal?
   (in-racket? (make-racket 330 245 0 0 335 240 false) 335 240)
   true
   "pointer must be inside racket"))

;; racket-selected? : Racket-> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the racket is selected
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the observer template of Racket
;; TESTS:
(begin-for-test
  (check-equal?
   (racket-selected? (make-racket 330 240 2 5 335 345 true))
   true
   "racket is selected")
  (check-equal?
   (racket-selected? (make-racket 330 240 2 5 335 345 false))
   false
   "racket is not selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene w) should return a canvas with
;; a ball and a racket, each at positions provided in world's data
;; DESIGN STRATEGY: Place racket and then ball in the scene

(define (world-to-scene w)
  (cond [(string=? (world-state w) "paused-state")
         (scene-with-ball (world-balls w)
                          (scene-with-racket (world-racket w) PAUSED-COURT))]
        [else (if (racket-selected? (world-racket w))
                  (scene-when-racket-selected w)
                  (scene-when-racket-notselected w))]))

;; scene-with-racket : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Place racket, then pointer and then ball in the scene

(define (scene-when-racket-selected w)
  (scene-with-ball (world-balls w)
                   (scene-with-pointer
                    (world-racket w)
                    (scene-with-racket (world-racket w)
                                       COURT))))

;; scene-with-racket : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Place racket and then ball in the scene

(define (scene-when-racket-notselected w)
  (scene-with-ball (world-balls w)
                   (scene-with-racket (world-racket w)
                                      COURT)))

;; scene-with-ball : BallList Scene -> Scene
;; GIVEN: a list of balls and a scene
;; RETURNS: a scene like the given one, but with the given ball painted on it.
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Using the HOF foldr on blist and scene

(define (scene-with-ball blist s)
  (foldr
   ;; Ball Scene -> Scene
   (lambda (x s)(place-image ball-image (ball-x x) (ball-y x) s))
   s blist))

#;
(define (scene-with-ball blist s)
  (cond
    [(empty? blist) s]
    [(empty? (rest blist))
     (place-image ball-image (ball-x (first blist)) (ball-y (first blist)) s)]
    [else
     (place-image ball-image (ball-x (first blist)) (ball-y (first blist))
                  (scene-with-ball (rest blist) s))]))

;; scene-with-racket : Racket Scene -> Scene
;; GIVEN: a racket and a scene
;; RETURNS: a scene like the given one, but with given racket painted on it.
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Place racket in the scene

(define (scene-with-racket r s)
  (place-image racket-image (racket-x r) (racket-y r) s))

;; scene-with-pointer : Racket Scene -> Scene
;; GIVEN: a racket and a scene
;; RETURNS: a scene like the given one, but with the given racket
;;          with a pointer circle painted on it.
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Place racket and pointer in the scene

(define (scene-with-pointer r s)
  (place-image mouse-pointer
               (+ (racket-x r) (racket-mx r))
               (+ (racket-y r)(racket-my r))
               s))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-to-scene (make-world (list (make-ball 300 646 2 5))
                               (make-racket 40 38 2 4 0 0 false)
                               1 TIMER paused-state))
   (scene-with-ball (list (make-ball 300 646 0 0))
                    (scene-with-racket
                     (make-racket 40 38 0 0 0 0 false) PAUSED-COURT))
   "world-to-scene")

  (check-equal?
   (world-to-scene (make-world (list (make-ball 300 246 2 5))
                               (make-racket 40 38 2 4 46 48 true)
                               1 TIMER rally-state))
   (scene-with-ball (list (make-ball 300 246 2 5))
                    (scene-with-pointer
                     (make-racket 40 38 2 4 46 48 true)
                     (scene-with-racket (make-racket 40 38 2 4 46 48 true)
                                        COURT)))
   "world-to-scene")

  (check-equal?
   (world-to-scene (make-world (list (make-ball 300 646 2 5))
                               (make-racket 40 38 2 4 0 0 false)
                               1 TIMER rally-state))
   (scene-with-ball (list (make-ball 300 646 2 5))
                    (scene-with-racket (make-racket 40 38 2 4 0 0 false)
                                       COURT))
   "world-to-scene")

  (check-equal?
   (scene-with-ball (list (make-ball 330 384 3 -9)
                          (make-ball 230 284 3 -9)) COURT)
   (place-image ball-image 330 384
                (scene-with-ball (list (make-ball 230 284 3 -9)) COURT))
   "scene-with-ball")
  
  (check-equal?
   (scene-with-ball (list) COURT)
   COURT
   "scene-with-ball")
  
  (check-equal?
   (scene-with-ball (list (make-ball 330 384 3 -9)) COURT)
   (place-image ball-image 330 384 COURT)
   "scene-with-ball")
  
  (check-equal?
   (scene-with-racket (make-racket 330 240 2 5 0 0 false) COURT)
   (place-image racket-image 330 240 COURT)
   "scene-with-racket"))
