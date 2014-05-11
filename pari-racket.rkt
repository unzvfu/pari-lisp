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

(define libpari (ffi-lib *path-to-libpari*))
(define-ffi-definer define-pari libpari)

(define-pari pari-init (_fun _size _ulong -> _void)
  #:c-id pari_init)
(define-pari pari-init-opts (_fun _size _ulong _ulong -> _void)
  #:c-id pari_init_opts)
(define-pari pari-close (_fun -> _void)
  #:c-id pari_close)
(define-pari pari-close-opts (_fun _ulong -> _void)
  #:c-id pari_close_opts)

(define-pari gen-to-str (_fun _pointer -> _bytes)
  #:c-id GENtostr)

;; FIXME: For some reason this function is very often (practically
;; every time) called for arguments to functions expecting _GEN's with
;; x *already* a _GEN.  Hence the guard in the first condition in the
;; COND form (which requires a forward reference to GEN?).
(define (scm-to-gen x)
  ;; FIXME: Handle scheme bignums (integer?) correctly
  (printf "SCM --> GEN with x = ~a~%" x)
  (cond [(GEN? x) x]
        [(zero? x) gen_0]
        [(fixnum? x) (stoi x)]
        [(flonum? x) (dbltor x)]
        [else (error x "cannot be coverted to a GEN")]))

(define (gen-to-scm x)
  (let ([s (gen-to-str x)])
    (printf "GEN --> SCM with x = (GEN) ~a~%" s)
    (free s))
  x)

(define-cpointer-type _GEN #f scm-to-gen gen-to-scm)

(define _pari_sp _ulong)

;; Pari needs to be initialised in order to access things like avma,
;; gen_0, etc.
(pari-init-opts (expt 2 24) 0 5)

(define-pari gen_0 _GEN)
(define-pari gen_1 _GEN)
(define-pari gen_m1 _GEN)
(define-pari gen_2 _GEN)

(define-c avma libpari _pari_sp)

(define-pari stoi (_fun _long -> _GEN))
(define-pari utoi (_fun _ulong -> _GEN))
(define-pari dbltor (_fun _double -> _GEN))
(define-pari output (_fun _GEN -> _void))

(define-pari gadd (_fun _GEN _GEN -> _GEN))
(define-pari gsub (_fun _GEN _GEN -> _GEN))
(define-pari gneg (_fun _GEN -> _GEN))
(define-pari gmul (_fun _GEN _GEN -> _GEN))
(define-pari gdiv (_fun _GEN _GEN -> _GEN))
(define-pari gsqr (_fun _GEN -> _GEN))

(define-pari factor (_fun _GEN -> _GEN))

(define (+ . args)
  (foldl gadd gen_0 args))

(define (* . args)
  (foldl gmul gen_1 args))
