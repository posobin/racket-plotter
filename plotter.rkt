#lang racket/gui
(define frame (new frame%
                   [label "Example"]
                   [width 500]
                   [height 500]))
; Finds root of the function on the given interval with given accuracy
; and draw chord in drawing context (dc). 
; Returns list consisiting of root value and list containing all the chords.
(define (find-root x0 x1 func epsilon dc)
  (define left x0)
  (define right x1)
  (when (negative? (func x0 epsilon))
    (set! left x1)
    (set! right x0))
  (define (x-intersection x0 x1) (/ (+ (* x0 (abs (func x1 epsilon))) (* x1 (abs (func x0 epsilon))))
                                    (+ (abs (func x0 epsilon)) (abs (func x1 epsilon)))))
  (define chords-list (list))
  (for ([i (in-naturals)]
        #:break (< (abs (func (x-intersection left right) epsilon)) epsilon))
    ;(displayln (func (x-intersection left right)))
    (set! chords-list (append chords-list (list(list left (func left epsilon) right (func right epsilon)))))
    (if (positive? (x-intersection left right))
        (set! right (x-intersection left right))
        (set! left (x-intersection left right))))
  (displayln (real->decimal-string (func (x-intersection left right) epsilon) 10))
  (list (x-intersection left right) chords-list))
; Integrate func from x0 to x1 with given accuracy (epsilon)
(define (integrate x0 x1 func epsilon)
  (define value1 0)
  (define value2 +inf.0)
  (define number-of-parts 3)
  (define n 1)
  ; Height (horizontal length) of a single trapezium
  (define length (/ (- x1 x0) n))
  (define previous-value (func x0))
  ; Recount squares while the difference between them is more than epsilon
  (for/last ([i (in-naturals)]
             #:break (< (abs (- value2 value1)) epsilon))
    (begin 
      (set! value1 value2)
      (set! value2 0)
      (set! n (* number-of-parts n))
      (set! previous-value (func x0))
      (set! length (/ (- x1 x0) n))
      ; Count new square by adding up trapeziums' squares
      (for ([i (in-range 1 (+ n 1))])
        (begin (set! value2
                     (+ value2
                        (* (/ (+ previous-value (func (+ x0 (* length i)))) 2)
                           length)))
               (set! previous-value (func (+ x0 (* length i))))
               ))
      )
    )
  value2
  )
                    
; Plot graph on dc (drawing context) given by func from a to b 
; onto the rectangle with coordinates (x1, y1) (x2, y2).
; Also draw chords from chords-list on top and mark one root
(define (plot-graph dc func epsilon a b x1 y1 x2 y2 chords-list root-value)
  ; Convert x-coordinate on the screen to the domain coordinate
  (define (x-to-domain x)
    (+ a 
       (* (- b a)
          (/ (- x x1) 
             (- x2 x1))))
    )
  ; Convert value from domain to x-coordinate
  (define (domain-to-x value)
    (+ x1
       (* (- x2 x1)
          (/ (- value a)
             (- b a)))))
  ; Convert value from range to the screen coordinate
  (define (range-to-y value min-value max-value)
    (- y2
       (* (- y2 y1)
          (/ (- value min-value)
             (- max-value min-value))))
    )
  ; Convert on-screen coordinate to value in range
  (define (y-to-range y min-value max-value)
    (+ min-value
       (* (- max-value min-value)
          (/ (- y2 y)
             (- y2 y1)))))
  ; Make list of function values at every point that corresponds to the pixel
  (define function-values
    (for/list ([i (in-range x1 (+ x2 1))]) 
      (func (x-to-domain i) epsilon)
      ))
  (define max-value (apply max function-values))
  (define min-value (apply min function-values))
  ; Calculate on-screen y-coordinates of calculated values
  (define plot-values
    (if (= max-value min-value)
        (map (lambda (val) (/ (+ y1 y2) 2)) function-values)
        (map (lambda (val) (range-to-y val min-value max-value))
             function-values)
        )
    )
  ; Plot graph, connecting calculated points
  (for ([i (in-range x1 x2)]
        [value plot-values]
        [next-value (list-tail plot-values 1)])
    (send dc draw-line i value (+ i 1) next-value))
  
  (define arrow-size 5)
  
  ; Draw x-axis
  (define x-axis-y (if (and (<= min-value 0) (<= 0 max-value))
                       (range-to-y 0 min-value max-value)
                       y2))
  (send dc draw-line x1 x-axis-y x2 x-axis-y)
  (send dc draw-line x2 x-axis-y (- x2 arrow-size) (+ x-axis-y arrow-size))
  (send dc draw-line x2 x-axis-y (- x2 arrow-size) (- x-axis-y arrow-size))
  (send dc draw-text "x" x2 x-axis-y)
  
  ; Draw y-axis
  (define y-axis-x (if (and (<= a 0) (<= 0 b))
                       (domain-to-x 0)
                       x1))
  (send dc draw-line y-axis-x y1 y-axis-x y2)
  (send dc draw-line y-axis-x y1 (- y-axis-x arrow-size) (+ y1 arrow-size))
  (send dc draw-line y-axis-x y1 (+ y-axis-x arrow-size) (+ y1 arrow-size))
  (send dc draw-text "y" (- y-axis-x (+ arrow-size 10)) y1)
  
  (for ([chord chords-list])
    (send dc draw-line 
          (domain-to-x (list-ref chord 0))
          (range-to-y (list-ref chord 1) min-value max-value)
          (domain-to-x (list-ref chord 2))
          (range-to-y (list-ref chord 3) min-value max-value)))
  (unless (infinite? root-value)
    (send dc draw-text (real->decimal-string root-value 6)
          (domain-to-x root-value)
          (+ x-axis-y arrow-size)))
  
  ;(define points-distance 50)
  ;(for ([i (in-range x1 (- x2 points-distance) points-distance)])
  ;  (define beautiful-text
  ;  (send dc draw-text (number->string (x-to-domain i)) i (+ y2 arrow-size))))
    ;(display (exact->inexact (x-to-arg i))))
  )
(define (func x epsilon)
  (- (* 2 x) (integrate -0.99 x 
                        (lambda (y) (/ (* (+ y 2) (sqrt (log (+ y 2)))))) 
                        (/ epsilon 10)))
  )
(define solve #f)
(define chords-list (list))
(define root +inf.0)
(define main-panel (new horizontal-panel% [parent frame] [alignment '(right center)]))
(define left-value 0)
(define right-value 10)
(define left-chord-point +inf.0)
(define right-chord-point +inf.0)
(define epsilon 0.1)
(define default-left-value left-value)
(define default-right-value right-value)
(define default-left-chord-point left-chord-point)
(define default-right-chord-point right-chord-point)
(define default-epsilon epsilon)
(define x1 20)
(define y1 10)
(define x2 500)
(define y2 500)
(define (get-left-value unnecessary-argument-in-order-to-break-continuations)
  (if (or (real? left-value)
          (equal? (string->number (send left-value get-value)) #f))
      default-left-value
      (string->number (send left-value get-value))))
(define (get-right-value unnecessary-argument-in-order-to-break-continuations)
  (if (or (real? right-value)
          (equal? (string->number (send right-value get-value)) #f))
      default-right-value
      (string->number (send right-value get-value))))
(define (get-left-chord-point unnecessary-argument-in-order-to-break-continuations)
  (if (or (real? left-chord-point)
          (equal? (string->number (send left-chord-point get-value)) #f))
      default-left-chord-point
      (string->number (send left-chord-point get-value))))
(define (get-right-chord-point unnecessary-argument-in-order-to-break-continuations)
  (if (or (real? right-chord-point)
          (equal? (string->number (send right-chord-point get-value)) #f))
      default-right-chord-point
      (string->number (send right-chord-point get-value))))
(define (get-epsilon unnecessary-argument-in-order-to-break-continuations)
  (if (or (real? epsilon)
          (equal? (string->number (send epsilon get-value)) #f))
      default-epsilon
      (string->number (send epsilon get-value))))

(define graph
  (new canvas% [parent main-panel]
       [min-width 500]
       [min-height 500]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 1 1)
          (send dc set-pen "black" 1 'transparent)
          (send dc draw-rectangle x1 y1 x2 y2)
          (send dc set-pen "black" 1 'solid)
          (set! chords-list (list))
          (set! root +inf.0)
          (define ret '())
          (when solve
              (set! ret (find-root (get-left-chord-point 0) (get-right-chord-point 0) 
                                    func (get-epsilon 0) dc))
               (set! root (list-ref ret 0))
               (set! chords-list (list-ref ret 1)))
          (plot-graph dc func (get-epsilon 0) (get-left-value 0) (get-right-value 0)
                      x1 y1 x2 y2 chords-list root)
          )]))
(define text-fields-panel (new vertical-panel% [parent main-panel]))
(set! left-value
      (new text-field% 
           [parent text-fields-panel] 
           [label "left value"]
           [callback (lambda (t e) (set! solve #f)
                       )]
           [stretchable-width #f]))
(set! right-value
      (new text-field% 
           [parent text-fields-panel] 
           [label "right value"]
           [callback (lambda (t e) (set! solve #f)
                       )]
           [stretchable-width #f]))
(set! left-chord-point
      (new text-field%
           [parent text-fields-panel]
           [label "left chord point"]
           [callback (lambda (t e) (set! solve #f)
                       )]
           [stretchable-width #f]))
(set! right-chord-point
      (new text-field%
           [parent text-fields-panel]
           [label "right chord point"]
           [callback (lambda (t e) (set! solve #f)
                       )]
           [stretchable-width #f]))
(set! epsilon
      (new text-field%
           [parent text-fields-panel]
           [label "epsilon"]
           [callback (lambda (t e) (set! solve #f)
                       )]
           [stretchable-width #f]))
(new button%
     [parent text-fields-panel]
     [label "Plot"]
     [callback (lambda (button event)
                 (set! solve #f)
                 (send graph refresh-now))])
(define solve-button
  (new button%
       [parent text-fields-panel]
       [label "Solve it!"]
       [callback (lambda (button event)
                   (set! solve #t)
                   (send graph refresh-now))]))

(send graph refresh-now)

(send frame show #t)