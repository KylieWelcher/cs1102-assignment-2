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

(define-struct CowWorld (text cows TrafficLights hay time))

; Data definition for CowWorld
; interp. Represents a display containing text describing the state of the world, a list of cows displayed as images, and traffic lights monitoring the cows' movement
; text: the status board containing the number of awake cows, sleeping cows, bales of hay, whether cows are stampeding, and time elapse. Image type
; cows: ListOfCow type representing all the cows currently on the screen
; TrafficLights: Image representing the status of traffic. Will be three circles (red, yellow, green in that order) on top of one another. At any given point in time, exactly one bulb will be lit. The lit bulb represents the state of the traffic. Red means awake cows stop when they get to the light, yellow means awake cows to the left of the traffic light go half speed, green means awake cows move normally.
; hay: Natural representing the number of hay bales in the CowWorld
; time: Integer type representing the time elapsed in the CowWorld in ticks
; CowWorld is (make-CowWorld Image ListOfCow Image)

; Template function for CowWorld
(define (fn-for-CowWorld CowWorld)
  (...
   (CowWorld-text CowWorld)
   (fn-for-loc (CowWorld-cows CowWorld))
   (CowWorld-TrafficLights CowWorld)
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
    (on-tick change)
    (to-draw render)
    (on-key handle-key)
    (on-mouse handle-mouse)))



; CowWorld -> CowWorld
;  !!!
; changes the world state
(define (change CowWorld) (make-CowWorld "a" "b" "c"))

; CowWorld -> Image
;  !!!
; draws the world state

(define (render CowWorld) MTS)

#;(define (render CowWorld)

    (StopLight CowWorld)
    place (StatBoard CowWorld) x y MTS
    (Cow CowWorld))

;;CowWorld -> Image
;;draws the StatBoard of current world state

+;;!!!

(define (StatBoard CowWorld ListOfCow)
  (above (text (append (NumSleeping ListOfCow) " of sleeeping cows" 24 "pink"))
         (text (append (NumAwake ListOfCow) " of awake cows" 24 "pink"))
         (text (append (CowWorld-hay CowWorld) " of hay bales" 24 "pink"))
         (text (append (Stampeding? ListOfCow) " Stampeding?" 24 "pink"))
         (text (append (ticks->seconds (CowWorld-time CowWorld)) " time elasped" 24 "pink"))))

;;!!!
;;ListOfCow -> Natural
;;produces number of sleeping cows (1) in consumed CowWorld state

(check-expect (NumSleeping empty) 0)
(check-expect (NumSleeping               
(check-expect (NumSleeping
               
;look up differences in equal funtions 

;(define (NumSleeping LOC) 0)

(define (NumSleeping LOC)
  (cond [(empty? LOC) 0]
        [else (if (= (Cow-state (first LOC)) 1)
                  (+ 1 (NumSleeping (rest LOC)))
                  (NumSleeping (rest LOC)))]))

;;!!!
;;ListOfCow -> Natural
;;produces number of awake cows (0) in consumed CowWorld state

(check-expect (NumAwake empty) 0)
(check-expect (NumAwake
(check-expect (NumAwake

;(define (NumAwake LOC) 0)

(define (NumAwake LOC)
  (cond [(empty? LOC) 0]
        [else (if (= (Cow-state (first LOC)) 0)
                  (+ 1 (NumAwake (rest LOC)))
                  (NumAwake (rest LOC)))]))


;;!!!
;;ListOfCow -> Boolean
;;produces true if concumed CowWorld states has stampeding cows (2)

(check-expect (Stampeding empty) false)
(check-expect (Stampeding
(check-expect (Stampeding
               
;(define (Stampeding? LOC) false)

(define (Stampeding? LOC)
  (cond [(empty? LOC) false]
        [(= (Cow-state (first LOC)) 2) true]
        [else (Stampeding? (rest LOC))]))
                             

;StopLight = generates light

;cow = takes in a cow of a certain type (0-2?) and returns a image

; CowWorld KeyEvent -> CowWorld
;  !!!
; draws the world state
(define (handle-key CowWorld ke) (make-CowWorld "a" "b" "c"))

; CowWorld MouseEvent -> CowWorld
;  !!!
; draws the world state
(define (handle-mouse CowWorld x y me)
  (cond [(mouse=? me "'left-down") ]
        [else CowWorld]

        ))
