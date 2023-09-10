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

(define-struct CowWorld (text ListOfCow TrafficLights))

; interp. Represents a display containing text describing the state of the world (text), a list of cows displayed as images, and traffic lights monitoring the cows' movement
; CowWorld is (make-CowWorld Image ListOfCow TrafficLights)
; !!


;!!! define ListOfCow
; ListOfCow is one of:
; - empty
; - (cons Cow ListOfCow)

(define-struct Cow (state x y speed))
; interp. A cow with a state, x- and y- positions and a speed.
; State represents one of the three Images, awake, asleep, and stampeding
; x- and y- positions are Natural types
; speed is a Natural 

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

(define (StatBoard CowWorld)
  (above (text (append (fn-for-nsc CowWorld) " of sleeeping cows" 24 "pink"))
         (text (append (fn-for-nac CowWorld) " of awake cows" 24 "pink"))
         (text (append (fn-for-nhb CowWorld) " of hay bales" 24 "pink"))
         (text (append (fn-for-yns CowWorld) " Stampeding?" 24 "pink"))
         (text (append (fn-for-time CowWorld) " time elasped" 24 "pink"))))


                             

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

        )
  