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

(provide read-function-descriptions)

(require "utils.rkt")

(define descpath "/home/hlaw/src/pari/src/desc/pari.desc")

(define (line->pair line)
  (let* ([idx (index-of (string->list line) #\:)]
         [title (substring line 0 idx)]
         [content (string-trim (substring line (add1 idx)))])
    (cons title content)))

(define (fndoc-acc x acc)
  (if (string-empty? x)
      ;; Only add a new list if the current head is not empty
      (if (empty? (car acc)) acc (cons '() acc))
      (cons (cons x (car acc)) (cdr acc))))

(define (group-at-blank-lines lines)
  (let ([res (foldl fndoc-acc '(()) lines)])
    ;; Remove empty list at front if there was one.  There will only
    ;; ever be one, and then only if there were blank lines at the end
    ;; of the file.
    (if (empty? (car res)) (cdr res) res)))

(define (field-acc x acc)
  (if (string-starts-with? x #\space)
      (cons (string-append (car acc)
                           (string #\newline)
                           (substring x 1))
            (cdr acc))
      (cons x acc)))

(define (join-at-field-continuation lines)
  (foldl field-acc '() lines))

(define (read-function-descriptions)
  (map (λ (x) (map line->pair x))
       (group-at-blank-lines
        (join-at-field-continuation
         (file->lines descpath)))))
