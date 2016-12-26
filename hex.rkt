#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require (only-in srfi/1 fold))

(struct hex (x y) #:transparent)

(struct hex-board (width height) #:transparent)

; 0,0   1,0   2,0
;    0,1   1,1   2,1 
; 0,2   1,2   2,2
;    0,3   1,3   2,3
(define/match (neighbors board a-hex)
  [((hex-board width height) (hex x y))
   (let* ([y-up (modulo (- y 1) height)]
          [y-down (modulo (+ y 1) height)]
          [x-left (modulo (- x 1) width)]
          [x-right (modulo (+ x 1) width)]
          [x-over (if (even? y) x-left x-right)])
     (list
      (hex x y-up) (hex x-over y-up)
      (hex x-left y) (hex x-right y)
      (hex x y-down) (hex x-over y-down)))])


;;; Map display
(define hex-side 10)
(define hex-width (* 2 hex-side (/ (sqrt 3) 2)))
(define hex-row-height (* 3/2 hex-side))

(define sea-hex (rotate 90 (regular-polygon hex-side 6 "solid" "blue")))
(define land-hex (rotate 90 (regular-polygon hex-side 6 "solid" "green")))


(define (render-hex-board board render-hex)
  (fold
   (lambda (y image)
     (overlay/align/offset "left" "top"
      image
      (if (odd? y) (/ hex-width 2) 0) (* y hex-row-height)
      (fold
       (lambda (x image)
         (beside image (render-hex x y)))
       empty-image
       (range (hex-board-width board)))))
   empty-image
   (range (hex-board-height board))))

(define (map-editor width height land)
  (define (handle-mouse state mx my e)
    (if (equal? e "button-up")
        (let ([x (- (exact-round (+ (/ mx hex-width) (/ hex-width 4))) 5)]
              [y (exact-round (/ (- my 5) hex-row-height))])
          (print (list x y "from" mx my))
          (if (and (< 0 x width) (< 0 y height))
              (let ([h (hex x y)])
                (if (member h state)
                    (remove h state)
                    (cons h state)))
              state))
        state))
  
  (define (draw state)
    (render-hex-board (hex-board width height)
                      (lambda (x y)
                        (if (member (hex x y) state)
                            land-hex
                            sea-hex))))
  
  (big-bang land
            (to-draw draw)
            (on-mouse handle-mouse)))

(define (earth-editor)
  (map-editor 40 20 earth))

(define (boat-world)
  (define width 40)
  (define height 20)
  (define (draw state)
    (render-hex-board (hex-board width height) (lambda (x y)
                                                 (let ((bg (if (member (hex x y) earth)
                                                               land-hex
                                                               sea-hex)))
                                                   (if (equal? (hex x y) state)
                                                       (overlay (circle 5 "solid" "red") bg)
                                                       bg)))))
  (define (move-boat location)
    (let* ([hexes (neighbors (hex-board width height) location)]
           [water-hexes (filter (lambda (hex) (not (member hex earth))) hexes)])
      (random-element water-hexes)))
           
  (big-bang (hex 13 4)
            (on-tick move-boat 1/8)
            (to-draw draw)))

    
(define (random-element list)
  (list-ref list (random (length list))))

(define earth
  (list (hex 19 11) (hex 21 11) (hex 20 11) (hex 24 13) (hex 19 9) (hex 19 8) (hex 18 9) (hex 19 10) (hex 23 16) (hex 22 15) (hex 22 14) (hex 22 12) (hex 1 1) (hex 1 2) (hex 4 5) (hex 3 5) (hex 4 6) (hex 4 4) (hex 3 3) (hex 4 3) (hex 5 1) (hex 34 1) (hex 31 1) (hex 10 16) (hex 9 13) (hex 9 12) (hex 8 1) (hex 12 8) (hex 2 8) (hex 6 2) (hex 6 1) (hex 7 2) (hex 7 1) (hex 8 2) (hex 7 3) (hex 7 4) (hex 6 4) (hex 3 1) (hex 5 2) (hex 5 3) (hex 6 3) (hex 5 4) (hex 6 6) (hex 6 5) (hex 5 5) (hex 10 6) (hex 9 5) (hex 9 4) (hex 8 4) (hex 9 3) (hex 10 3) (hex 10 4) (hex 8 5) (hex 7 5) (hex 7 6) (hex 7 7) (hex 8 7) (hex 5 7) (hex 6 7) (hex 5 6) (hex 4 2) (hex 2 2) (hex 3 2) (hex 2 1) (hex 10 8) (hex 7 8) (hex 8 8) (hex 8 9) (hex 9 10) (hex 11 18) (hex 10 17) (hex 13 12) (hex 12 11) (hex 11 11) (hex 12 12) (hex 12 13) (hex 11 13) (hex 11 12) (hex 11 14) (hex 10 10) (hex 11 10) (hex 10 11) (hex 9 11) (hex 10 13) (hex 10 12) (hex 10 14) (hex 10 15) (hex 11 16) (hex 11 15) (hex 12 14) (hex 15 1) (hex 16 1) (hex 16 2) (hex 18 2) (hex 20 3) (hex 37 11) (hex 39 8) (hex 39 7) (hex 37 5) (hex 38 6) (hex 36 7) (hex 39 4) (hex 39 5) (hex 36 8) (hex 36 13) (hex 39 17) (hex 38 16) (hex 37 15) (hex 36 15) (hex 36 16) (hex 32 13) (hex 37 16) (hex 26 14) (hex 26 11) (hex 36 4) (hex 35 4) (hex 33 5) (hex 32 5) (hex 32 6) (hex 31 5) (hex 33 6) (hex 34 6) (hex 34 5) (hex 35 6) (hex 35 5) (hex 33 8) (hex 34 7) (hex 33 4) (hex 31 3) (hex 30 3) (hex 32 4) (hex 32 3) (hex 33 3) (hex 34 3) (hex 35 3) (hex 36 3) (hex 37 3) (hex 34 4) (hex 32 7) (hex 33 7) (hex 30 7) (hex 30 6) (hex 29 5) (hex 30 5) (hex 31 6) (hex 29 3) (hex 29 4) (hex 30 4) (hex 31 4) (hex 31 7) (hex 30 8) (hex 29 7) (hex 29 6) (hex 27 4) (hex 26 3) (hex 26 4) (hex 28 4) (hex 28 5) (hex 27 5) (hex 26 5) (hex 26 7) (hex 27 6) (hex 25 6) (hex 25 4) (hex 25 5) (hex 24 5) (hex 25 7) (hex 24 6) (hex 22 5) (hex 23 5) (hex 22 6) (hex 24 2) (hex 24 3) (hex 25 3) (hex 24 4) (hex 23 4) (hex 23 2) (hex 22 4) (hex 21 5) (hex 20 5) (hex 20 6) (hex 20 10) (hex 22 13) (hex 20 8) (hex 23 10) (hex 23 9) (hex 23 13) (hex 24 12) (hex 23 11) (hex 27 2) (hex 26 2) (hex 27 3) (hex 28 3) (hex 39 2) (hex 33 2) (hex 34 2) (hex 37 2) (hex 36 2) (hex 35 2) (hex 32 2) (hex 30 2) (hex 31 2) (hex 29 2) (hex 27 7) (hex 32 8) (hex 32 9) (hex 31 8) (hex 30 10) (hex 29 9) (hex 30 9) (hex 29 8) (hex 28 7) (hex 27 9) (hex 27 8) (hex 26 9) (hex 26 8) (hex 25 10) (hex 24 9) (hex 25 8) (hex 24 8) (hex 22 8) (hex 23 8) (hex 22 9) (hex 20 9) (hex 21 8) (hex 21 9) (hex 24 10) (hex 24 11) (hex 23 15) (hex 25 11) (hex 25 12) (hex 24 14) (hex 22 10) (hex 22 11) (hex 23 12) (hex 23 14) (hex 21 10) (hex 33 10) (hex 35 13) (hex 33 11) (hex 34 8) (hex 33 9) (hex 34 10) (hex 35 8) (hex 35 9) (hex 35 7) (hex 37 6) (hex 36 5) (hex 36 6) (hex 37 4) (hex 38 4) (hex 38 3) (hex 39 3) (hex 38 2) (hex 39 1) (hex 33 13)))
