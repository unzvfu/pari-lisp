;; Copyright 2014 Hamish Ivey-Law
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

(provide index-of
         string-index-of
         string-split-at
         member?)

;; FIXME: It's hard to believe that this function doesn't exist
;; somewhere in Racket.  But
;;   http://stackoverflow.com/a/15871126
(define (index-of lst ele)
  (let loop ([lst lst] [idx 0])
    (cond [(empty? lst) #f]
          [(equal? (first lst) ele) idx]
          [else (loop (rest lst) (add1 idx))])))

(define (string-first str x)
  (for/first ([ch (in-string str)]
              #:when (char=? ch x))
    i))

(define (string-index-of str ch)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond [(= len i) #f]
            [(equal? (string-ref str i) ch) i]
            [else (loop (add1 i))]))))

(define (string-split-at str ch)
  (let ([idx (string-index-of str ch)])
    (if (not idx)
        idx
        (values (substring str 0 idx) (substring str (add1 idx))))))

(define (member? target seq)
  (sequence-ormap (lambda (curr) (equal? curr target)) seq))

