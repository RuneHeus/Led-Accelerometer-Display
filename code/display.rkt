#lang racket
;------Besturingsystemen:taak->raspberry-pi-------------

(require "../raspi-gpio.rkt")

(require "driver.rkt")

(require "procedures.rkt")

(gpio-setup)

(gpio-mcp23017-setup 100 #x20)

(do ((i 0 (+ i 1)))
  ((= i 16))
  (gpio-set-pin-mode (+ 100 i) 'output))

(gpio-mcp3008-setup spi-channel)

;(display-loop) ;Procedure voor de patronen weer te geven

;(calibrate-loop) ;Calibreert de waarde voor de accelerometer
;(loop-bal) ;Loop voor de accelerometer


(define lsm303d (connect-lsm303d))

(define (loop-temp) ; Loop voor weergave van de temperatuur
  (display "Temperatuur: ")
  (displayln ((lsm303d 'lsm303d-temperature)))
  (gpio-delay-ms 300)
  (loop-temp))

(loop-temp)


