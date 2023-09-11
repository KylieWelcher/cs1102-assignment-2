;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Kylie and Sumanth

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28) ;; how many times on-tick is called per second
(define DISPLAY-X 150) ;; x coordinate of world's status
(define DISPLAY-Y 100) ;; y coordinate of world's status


(define WAKE-COW 0)
(define SLEEPING-COW 1) 
(define STAMPEDING-COW 2)

;;----------------------------------------------------------------------------
;; constants for how the traffic signal behaves.  The SA should be able
;; to modify these and your program should work ok.  Don't worry about our
;; changing LIGHT-RADIUS to a large number and you having to change where
;; cows and the status board appear. We won't stretch your code that far.

(define LIGHT-RADIUS 30) ;; Radius of the bulbs
(define GREEN-LENGTH 5) ;; How long the green bulb is lit
(define YELLOW-LENGTH 5) ;; How long the yellow bulb is lit
(define RED-LENGTH 4) ;; How long the red bulb is lit
;;----------------------------------------------------------------------------
(define SIGNAL-X (/ WIDTH 2))  ;; x coordinate of traffic signal
(define SIGNAL-Y 100) ;; y coordinate of traffic signal

;; The town bought a defective traffic signal
(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

;; To help synchronize the number of ticks (28/sec) with the traffic signal
;; (time in seconds)
;; Natural -> Natural
;; converts the number of clock ticks to # seconds as a natural
(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))
 
(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)

;; Yes, I really followed HtDF for the helper functions.  You should too.
;; Debugging is so much easier when you can trust the building blocks work properly

(define-struct CowWorld (cows hay time))

; Data definition for CowWorld
; interp. Represents a display containing text describing the state of the world, a list of cows displayed as images, and traffic lights monitoring the cows' movement
; cows: ListOfCow type representing all the cows currently on the screen
; hay: Natural representing the number of hay bales in the CowWorld
; time: Integer type representing the time elapsed in the CowWorld in ticks
; CowWorld is (make-CowWorld ListOfCow Number Number)

; Template function for CowWorld
(define (fn-for-CowWorld CowWorld)
  (...
   (fn-for-loc (CowWorld-cows CowWorld))
   (CowWorld-hay CowWorld)
   (CowWorld-time CowWorld)))



(define-struct Cow (state x y speed))


; Data definition for Cow
; interp. A cow with a state, x- and y- positions and a speed.
; state: A Natural type from 0 to 2 inclusive. 0 represents an awake cow, 1 represents a sleeping cow, 2 represents a stampeding cow
; x: Natural representing the x-coordinate of the cow
; y: Natural representing the y-coordinate of the cow 
; speed is a Natural representing the speed of the cow in pixels per tick

; Template function for Cow
(define (fn-for-Cow Cow)
  (...
   (Cow-state Cow)
   (Cow-x Cow)
   (Cow-y Cow)
   (Cow-speed Cow)))

(define C1 (make-Cow 0 20 20 2))
(define C2 (make-Cow 0 40 20 2))

(define C3 (make-Cow 1 60 40 0))
(define C4 (make-Cow 1 80 40 0))
(define C5 (make-Cow 0 20 40 1))

(define C6 (make-Cow 2 60 40 4))
(define C7 (make-Cow 2 80 40 4))
(define C8 (make-Cow 1 20 40 0))


; Data definition for ListOfCow
; ListOfCow is one of:
; - empty
; - (cons Cow ListOfCow)

; Template function for ListOfCow
(define (fn-for-loc loc)
  (cond [(empty? loc) ...]
        [else
         (...
          (fn-for-Cow (first loc))
          (fn-for-loc (rest loc)))]))
                   
(define (main CowWorld)
  (big-bang CowWorld
    (on-tick changeCowWorld)
    (to-draw render)
    (on-key handle-key)))
;(on-mouse handle-mouse)))

(define LOC1 (list C1 C2))
(define LOC2 (list C3 C4 C5))
(define LOC3 (list C6 C7 C8))
(define LOC4 (list C3 C4))

(define START (make-CowWorld LOC1 0 0))
(define START-2 (make-CowWorld LOC3 4 34))

; CowWorld -> CowWorld
;  !!!
; changes the world state
;(define (changeCowWorld CowWorld) (make-CowWorld "a" "b" "c"))

(define (changeCowWorld CowWorld)
  (make-CowWorld
   ...
   (StatBoard (CowWorld-time CowWorld) (CowWorld-hay CowWorld) (CowWorld-cows CowWorld))
   (+ (CowWorld-time CowWorld) 1)))
 
; CowWorld -> Image
;  !!!
; draws the world state

;(define (render CowWorld) MTS)

(define (render CowWorld)
  (place-image (StatBoard
                (CowWorld-time CowWorld)
                (CowWorld-hay CowWorld)
                (CowWorld-cows CowWorld)) (/ WIDTH 2) (/ HEIGHT 2) MTS))

;;CowWorld -> Image
;;draws the StatBoard of current world state

;;!!!

(define (StatBoard ticks hay ListOfCow)
  (above/align "left" (text (string-append (number->string (NumSleeping ListOfCow)) " of sleeeping cows") 24 "pink")
               (text (string-append (number->string (NumAwake ListOfCow)) " of awake cows") 24 "pink")
               (text (string-append (number->string hay) " of hay bales") 24 "pink")
               (text (string-append (boolean->string (Stampeding? ListOfCow)) " Stampeding?") 24 "pink")
               (text (string-append (number->string (ticks->seconds ticks)) " time elasped") 24 "pink")))

;;!!! 
;;ListOfCow -> Natural
;;produces number of sleeping cows (1) in consumed CowWorld state

(check-expect (NumSleeping empty) 0)
(check-expect (NumSleeping LOC1) 0)
(check-expect (NumSleeping LOC2) 2)
(check-expect (NumSleeping LOC3) 1)

;(check-expect (NumSleeping               
;(check-expect (NumSleeping
               
;look up differences in equal funtions 

;(define (NumSleeping LOC) 0)

(define (NumSleeping LOC)
  (cond [(empty? LOC) 0]
        [else (if (= (Cow-state (first LOC)) 1)
                  (+ 1 (NumSleeping (rest LOC)))
                  (NumSleeping (rest LOC)))]))

;;!!!
;;ListOfCow -> Natural
;;produces number of awake cows (state 0) in consumed CowWorld state

(check-expect (NumAwake empty) 0)
(check-expect (NumAwake LOC1) 2)  
(check-expect (NumAwake LOC2) 1)
(check-expect (NumAwake LOC3) 2)

;(define (NumAwake LOC) 0)

(define (NumAwake LOC)
  (cond [(empty? LOC) 0]
        [else (if (= (Cow-state (first LOC)) 1)
                  (NumAwake (rest LOC))
                  (+ 1 (NumAwake (rest LOC))))]))


;;!!!
;;ListOfCow -> Boolean
;;produces true if consumed CowWorld states has stampeding cows (2)

(check-expect (Stampeding? empty) false)
(check-expect (Stampeding? LOC1) false)
(check-expect (Stampeding? LOC2) false)
(check-expect (Stampeding? LOC3) true)
;(check-expect (Stampeding
;(check-expect (Stampeding
               
;(define (Stampeding? LOC) false)

(define (Stampeding? LOC)
  (cond [(empty? LOC) false]
        [(= (Cow-state (first LOC)) 2) true]
        [else (Stampeding? (rest LOC))]))
                             

;StopLight = generates light

;cow = takes in a cow of a certain type (0-2?) and returns a image

; CowWorld KeyEvent -> CowWorld
; Updates world state based on key event

;(define (handle-key CowWorld ke) CowWorld)
  
(define (handle-key CowWorld ke)
  (cond [(key=? "+" ke) (increaseHay CowWorld)]
        [(key=? "-" ke) (decreaseHay CowWorld)]
        [(key=? "w" ke) (make-CowWorld (wakeCow (CowWorld-cows CowWorld))
                                       (CowWorld-hay CowWorld)
                                       (CowWorld-time CowWorld))]
        [(key=? "s" ke) (make-CowWorld (sleepCow CowWorld)
                                       (CowWorld-hay CowWorld)
                                       (CowWorld-time CowWorld))]
        [else CowWorld]))

; CowWorld -> CowWorld
; returns a CowWorld with 1 greater bale of hay than the CowWorld that was inputted.
(check-expect (increaseHay START) (make-CowWorld LOC1 (+ 1 0) 0))
(check-expect (increaseHay START-2) (make-CowWorld LOC3 (+ 1 4) 34))

        
;(define (increaseHay CowWorld) CowWorld)                       
         
(define (increaseHay CowWorld) (make-CowWorld
                                (CowWorld-cows CowWorld)
                                (+ 1 (CowWorld-hay CowWorld))
                                (CowWorld-time CowWorld)))


; CowWorld -> CowWorld
; returns a CowWorld with 1 fewer bale of hay than the CowWorld that was inputted
; if 0 hay bales are in the CowWorld, keep it at 0. No negative hay bales allowed
(check-expect (decreaseHay START) START)
(check-expect (decreaseHay START-2) (make-CowWorld LOC3 (- 4 1) 34))

        
;(define (decreaseHay CowWorld) CowWorld)                       
         
(define (decreaseHay CowWorld)
  (cond [(= (CowWorld-hay CowWorld) 0) CowWorld]
        [else (make-CowWorld
               (CowWorld-cows CowWorld)
               (- (CowWorld-hay CowWorld) 1)
               (CowWorld-time CowWorld))]))

; ListOfCow -> ListOfCow
; Searches for an asleep cow in a list of cows and awakes it.
; If there are no asleep cows, do nothing.
(check-expect (wakeCow LOC1) LOC1)
(check-expect (wakeCow LOC2) (list (make-Cow 0 (Cow-x C3) (Cow-y C3) (Cow-speed C3)) C4 C5))
(check-expect (wakeCow LOC3) (list C6 C7 (make-Cow 0 (Cow-x C8) (Cow-y C8) (Cow-speed C8))))

;(define (wakeCow LOC) LOC)

(define (wakeCow LOC)
  (cond [(empty? LOC) empty]
        [else
         (if (= (Cow-state (first LOC)) 1)
             (cons (make-Cow 0 (Cow-x (first LOC)) (Cow-y (first LOC)) (Cow-speed (first LOC))) (rest LOC))
             (cons (first LOC) (wakeCow (rest LOC))))]))

; ListOfCow -> ListOfCow
; Searches for an awake cow in a list of cows and puts it to sleep.
; If there are no awake cows do nothing.
(check-expect (sleepCow LOC4) LOC4)
(check-expect (sleepCow LOC1) (list (make-Cow 1 (Cow-x C1) (Cow-y C1) (Cow-speed C1)) C2))
(check-expect (sleepCow LOC2) (list C3 C4 (make-Cow 1 (Cow-x C5) (Cow-y C5) (Cow-speed C5))))
(check-expect (sleepCow LOC3) (list (make-Cow 1 (Cow-x C6) (Cow-y C6) (Cow-speed C6)) C7 C8))

;(define (sleepCow LOC) LOC)

(define (sleepCow LOC)
  (cond [(empty? LOC) empty]
        [else
         (if (not (= (Cow-state (first LOC)) 1))
             (cons (make-Cow 1 (Cow-x (first LOC)) (Cow-y (first LOC)) (Cow-speed (first LOC))) (rest LOC))
             (cons (first LOC) (sleepCow (rest LOC))))]))

  

; CowWorld Natural Natural MouseEvent -> CowWorld
; adds an awake Cow to list of cows in the given CowWorld when a mouse left click occurs
; the coordinates of the new Cow are the coordinates (x, y) of the mouse event.
(check-expect (handle-mouse START 80 80 "button-up") START)
(check-expect (handle-mouse (make-CowWorld empty 0 0) 100 230 "button-down")
              (cons (make-Cow 0 100 230 (NumAwake empty)) empty))
(check-expect (handle-mouse START-2 200 170 "button-down")
              (cons (make-Cow 0 200 170 (NumAwake LOC3)) LOC3))

(define (handle-mouse CowWorld x y me) 
    (cond [(mouse=? me "button-down")
           (cons (make-Cow 0 x y (NumAwake (CowWorld-cows CowWorld))) (CowWorld-cows CowWorld))]
          [else CowWorld])) 
