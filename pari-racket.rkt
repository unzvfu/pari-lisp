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
;; "pari-racket.rkt"> (pari-init-opts (expt 2 24) 0 5)
;; "pari-racket.rkt"> (define a (utoi 8))
;; "pari-racket.rkt"> (define b (stoi -9))
;; "pari-racket.rkt"> (define c (gmul a (gsqr b)))
;; "pari-racket.rkt"> (define f (factor c))
;; "pari-racket.rkt"> (output f)
;; [2,3;3,4]


#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define *path-to-libpari* "/home/hlaw/local/lib/libpari.so")

(define-ffi-definer define-pari (ffi-lib *path-to-libpari*))

(define-pari pari-init (_fun _size _ulong -> _void)
  #:c-id pari_init)
(define-pari pari-init-opts (_fun _size _ulong _ulong -> _void)
  #:c-id pari_init_opts)
(define-pari pari-close (_fun -> _void)
  #:c-id pari_close)
(define-pari pari-close-opts (_fun _ulong -> _void)
  #:c-id pari_close_opts)

(define-cpointer-type _GEN) ; #f scm-to-gen output)

(define-pari stoi (_fun _long -> _GEN))
(define-pari utoi (_fun _ulong -> _GEN))
(define-pari dbltor (_fun _double -> _GEN))
(define-pari output (_fun _GEN -> _void))

(define (scm-to-gen x)
  ;; FIXME: Handle scheme bignums (integer?) correctly
  (cond ((fixnum? x) (stoi x))
        ((flonum? x) (dbltor x))
        (else (error x "cannot be coverted to a GEN"))))

(define-pari gadd (_fun _GEN _GEN -> _GEN))
(define-pari gsub (_fun _GEN _GEN -> _GEN))
(define-pari gneg (_fun _GEN -> _GEN))
(define-pari gmul (_fun _GEN _GEN -> _GEN))
(define-pari gdiv (_fun _GEN _GEN -> _GEN))
(define-pari gsqr (_fun _GEN -> _GEN))

(define-pari factor (_fun _GEN -> _GEN))
