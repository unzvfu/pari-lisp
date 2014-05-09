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
;; Usage: Fire up Racket then type
;;
;; -> ,en "pari-racket.rkt"
;;

#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define *path-to-libpari* "/home/hlaw/local/lib/libpari.so")

(define-ffi-definer define-pari (ffi-lib *path-to-libpari*))

(define-cpointer-type _GEN #f
  #f ; FIXME: Add this
  (lambda (x)  ; FIXME: Use OUTPUT instead
    (error '_GEN "cannot be used as an output type")))

(define-pari pari-init (_fun _size _ulong -> _void)
  #:c-id pari_init)
(define-pari pari-init-opts (_fun _size _ulong _ulong -> _void)
  #:c-id pari_init_opts)
(define-pari pari-close (_fun -> _void)
  #:c-id pari_close)
(define-pari stoi (_fun _long -> _GEN))
(define-pari utoi (_fun _ulong -> _GEN))
(define-pari output (_fun _GEN -> _void))

(define-pari gadd (_fun _GEN _GEN -> _GEN))
(define-pari gsub (_fun _GEN _GEN -> _GEN))
(define-pari gneg (_fun _GEN -> _GEN))
(define-pari gmul (_fun _GEN _GEN -> _GEN))
(define-pari gdiv (_fun _GEN _GEN -> _GEN))
(define-pari gsqr (_fun _GEN -> _GEN))

(define-pari factor (_fun _GEN -> _GEN))

;; (pari-init-opts 16000000 0 5)
;; (define a (utoi 8))
;; (define b (stoi -9))
;; (define c (gmul a (gsqr b)))
;; (define f (factor c))
