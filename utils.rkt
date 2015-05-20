;; Copyright 2014, 2015 Hamish Ivey-Law
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;
;; This file contains utility functions of general interest.
;;

#lang racket

(provide string-index-of
         string-empty?
         string-starts-with?
         string-reverse)

(define (string-index-of str ch)
  (for/or ([idx (in-range (string-length str))]
           #:when (eq? (string-ref str idx) ch))
    idx))

(define (string-empty? s)
  (string=? s ""))

(define (string-starts-with? s ch)
  (if (string-empty? s)
      #f
      (char=? ch (string-ref s 0))))

;; Somewhat strangely, the implementation used is faster than this
;; commented-out one by around 10-30% on average.
;;
;; (define (string-reverse-2 s)
;;   (let* ([len (string-length s)]
;;          [t (make-string len)])
;;     (for ([i (in-range len)])
;;       (string-set! t i (string-ref s (- len i 1))))
;;     t))
(define (string-reverse s)
  (let ([t (string-copy s)]
        [len (string-length s)])
    (for ([i (in-range (/ len 2))])
      (string-set! t i (string-ref s (- len i 1)))
      (string-set! t (- len i 1) (string-ref s i)))
    t))
