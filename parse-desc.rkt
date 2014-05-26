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

#lang racket

(define descpath "/home/hlaw/src/pari/src/desc/pari.desc")

(define (field-print f port mode)
  (write-string (format "~a: ~a" (field-title f) (field-content f))
                port))

(struct field (title content)
        #:methods gen:custom-write
        [(define write-proc field-print)])

;; FIXME: It's hard to believe that this function doesn't exist
;; somewhere in Racket.  But
;;   http://stackoverflow.com/a/15871126
(define (index-of lst ele)
  (let loop ([lst lst] [idx 0])
    (cond [(empty? lst) #f]
          [(equal? (first lst) ele) idx]
          [else (loop (rest lst) (add1 idx))])))

(define (line->field line)
  (let* ([idx (index-of (string->list line) #\:)]
         [title (substring line 0 idx)]
         [content (string-trim (substring line (add1 idx)))])
    (field title content)))

(define (next-func-desc lines)
  (let-values ([(desc rest)
                (splitf-at lines
                           (lambda (line)
                             (not (= (string-length line) 0))))])
    (values desc (drop rest 1))))

(define (starts-with? str ch)
  (equal? (string-ref str 0) ch))

(define (remove-head lst)
  (values (car lst) (cdr lst)))

(define (remove-tail lst)
  (let-values ([(beginning last) (split-at-right lst 1)])
    (values beginning (car last))))

(define (normalise-func-desc first-bit last-bit)
  (if (empty? last-bit)
      first-bit
      (let-values ([(elt rest) (remove-head last-bit)])
        (normalise-func-desc
         (if (starts-with? elt #\space)
             (let-values ([(beginning tail) (remove-tail first-bit)])
               (append beginning
                       (list (string-join (list tail (substring elt 1))
                                          (string #\newline)))))
             (append first-bit (list elt)))
         rest))))

(define (stream->list fn lines)
  (define (iter acc rest)
    (if (empty? rest)
        acc
        (let-values ([(next tail) (fn rest)])
          (iter (append acc (list next)) tail))))
  (iter '() lines))

(define (split-desc lines)
  (stream->list next-func-desc lines))

(define (file->fields fname)
  (let ([lines (file->lines fname)])
    fname))
