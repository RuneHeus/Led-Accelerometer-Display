#lang racket

(require "../raspi-gpio.rkt")

(provide (all-defined-out))


;------------DISPLAY---------------------------
(define (clear-display)
  (gpio-digital-write 100 0)
  (gpio-digital-write 101 0)
  (gpio-digital-write 102 0)
  (gpio-digital-write 113 0)
  (gpio-digital-write 114 0)
  (gpio-digital-write 115 0))

(define pattern (list
                 (list 1 0 1)
                 (list 0 1 0)
                 (list 1 0 1)))

(define pattern2 (list
                  (list 1 1 1)
                  (list 0 0 0)
                  (list 1 1 1)))

(define pattern3 (list
                  (list 1 0 1)
                  (list 1 0 1)
                  (list 1 0 1)))

(define (draw-pattern pattern)
  (define (iter row lijst)
    (when (not (null? lijst))
      (clear-display)
      (when (member 1 (car lijst))
        (gpio-digital-write (+ 100 row) 1))
      (map (lambda (column)
             (gpio-digital-write (+ 113 column) 1))
           (list-0-place (car lijst)))
      (gpio-delay-ms 5)
      (iter (+ row 1) (cdr lijst))))
  (iter 0 pattern))

(define (list-0-place lst)
  (define (iter count lijst 0list)
    (if (not (null? lijst))
        (begin 
          (when (= (car lijst) 0)
            (set! 0list (add-element-to-list 0list count)))
          (iter (+ count 1) (cdr lijst) 0list))
        0list))
  (iter 0 lst '()))

(define (add-element-to-list lst element)
  (if (null? lst)
      (list element)
      (append lst (list element))))

(define (loop1)
  (draw-pattern pattern))

(define (loop2)
  (draw-pattern pattern2))

(define (loop3)
  (draw-pattern pattern3))

(define (display-loop)
  (cond ((< (gpio-elapsed-ms) 5000) (begin (loop1) (display-loop)))
        ((< (gpio-elapsed-ms) 10000) (begin (loop2) (display-loop)))
        ((< (gpio-elapsed-ms) 15000) (begin (loop3) (display-loop)))
        (else (clear-display))))

;-----------------------------------------------------------------

;-----------------------ACCELEROMETER-----------------------------

(define X_CH 0)
(define Y_CH 1)
(define Z_CH 2)

(define spi-channel 0)
(define channel-config-single 8)
(define channel-config-diff 0)
(define channel-config channel-config-single)

(define sample-size 10)

(define (read-axis ch)
  (let ((reading 0))
    (gpio-mcp3008-analog-read spi-channel channel-config ch)
    (gpio-delay-ms 1)
    (for ([i sample-size])
      (set! reading
            (+ reading (gpio-mcp3008-analog-read spi-channel channel-config ch))))
    (/ reading (exact->inexact sample-size))))

(define x-raw-min 512)
(define y-raw-min 512)
(define z-raw-min 512)

(define x-raw-max 512)
(define y-raw-max 512)
(define z-raw-max 512)

(define x-av 514.5)
(define y-av 519.5)
(define z-av 0)

(define (auto-calibrate x-raw y-raw z-raw)
  (cond ((< x-raw x-raw-min) (set! x-raw-min x-raw)))
  (cond ((> x-raw x-raw-max) (set! x-raw-max x-raw)))
  (cond ((< y-raw y-raw-min) (set! y-raw-min y-raw)))
  (cond ((> y-raw y-raw-max) (set! y-raw-max y-raw)))
  (cond ((< z-raw z-raw-min) (set! z-raw-min z-raw)))
  (cond ((> z-raw z-raw-max) (set! z-raw-max z-raw))))

(define (calibrate-loop)
  (let ((x-raw (read-axis X_CH))
        (y-raw (read-axis Y_CH))
        (z-raw (read-axis Z_CH)))
    (for ([i 400])
      (set! x-raw (read-axis X_CH))
      (set! y-raw (read-axis Y_CH))
      (set! z-raw (read-axis Z_CH))
      (auto-calibrate x-raw y-raw z-raw)
      (gpio-delay-ms 50))))

(define (value-map x in-min in-max out-min out-max)
  (/ (* (- x in-min) (- out-max out-min))
     (+ (- in-max in-min) out-min)))

(define bal-pattern (list
                     (list 0 0 0)
                     (list 0 1 0)
                     (list 0 0 0)))

(define x-max-length 2)
(define y-max-length 2)

(define bal-pos-i 1)
(define bal-pos-j 1)

(define (change-at l x y to)
  (for/list ([row l] [i (length l)])
    (for/list ([e row] [j (length row)])
      (if (and (= x i) (= y j))
          to
          e))))

(define (draw-ball new-bal-pos-i new-bal-pos-j)
  (set! bal-pattern (change-at bal-pattern bal-pos-i bal-pos-j 0))
  (when (and (>= new-bal-pos-i 0) (<= new-bal-pos-i 2))
    (set! bal-pos-i new-bal-pos-i))
  (when (and (>= new-bal-pos-j 0) (<= new-bal-pos-j 2))
    (set! bal-pos-j new-bal-pos-j))
  (set! bal-pattern (change-at bal-pattern bal-pos-i bal-pos-j 1))
  (draw-pattern bal-pattern))

(define (loop-bal)
  (let* ((x-raw (read-axis X_CH))
         (y-raw (read-axis Y_CH))
         (z-raw (read-axis Z_CH))
         ; convert raw values to "milli-Gs"
         (x-scaled (value-map x-raw x-raw-min x-raw-max -1000 1000))
         (y-scaled (value-map y-raw y-raw-min y-raw-max -1000 1000))
         (z-scaled (value-map z-raw z-raw-min z-raw-max -1000 1000))
         (x-accel (/ x-scaled 1000))
         (y-accel (/ y-scaled 1000))
         (z-accel (/ z-scaled 1000)))
    (when (> x-raw (+ x-av 10))
      (display "Left")
      (draw-ball bal-pos-i (- bal-pos-j 1))
      (newline))
    (when (< x-raw (- x-av 10))
      (display "Right")
      (draw-ball bal-pos-i (+ bal-pos-j 1))
      (newline))
    (when (< y-raw (- y-av 10))
      (display "Up")
      (draw-ball (- bal-pos-i 1) bal-pos-j)
      (newline))
    (when (> y-raw (+ y-av 10))
      (display "Down")
      (draw-ball (+ bal-pos-i 1) bal-pos-j)
      (newline))
    (when (and (< x-raw (+ x-av 10)) (> x-raw (- x-av 10)) (> y-raw (- y-av 10)) (< y-raw (+ y-av 10)))
      (display "Center")
      (draw-ball 1 1)
      (newline))
    ;    (display " X-raw: ")
    ;    (display x-raw)
    ;    (display " Y-raw: ")
    ;    (display y-raw)
    ;    (display " Z-raw: ")
    ;    (display z-raw)
    ;    (newline)
    (gpio-delay-ms 5)
    (loop)))


;-----------------------------------------------------------------