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


#lang racket
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

(define (gen-print x port mode)
  (write-string (gen-to-string x) port))

(struct gen-hdl (ref [str #:auto #:mutable])
        #:auto-value #f
        ;; ref must be a GEN.
        #:guard (lambda (ref type-name)
                  (cond [(GEN? ref) ref]
                        [else (error type-name "bad gen ref: ~e" ref)]))
        #:methods gen:custom-write
        [(define write-proc gen-print)])

(define (gen-to-string x)
  ;; FIXME: I'm supposed to free the result of GENtostr at some point.
  (when (not (gen-hdl-str x))
    (set-gen-hdl-str! x (GENtostr x)))
  (gen-hdl-str x))

;; Pari stack
(define _pari_sp _ulong)
(define-c avma libpari _pari_sp)

;; Conversion routines return _pointers.
(define-pari stoi (_fun _long -> _pointer))
(define-pari utoi (_fun _ulong -> _pointer))  ; Unnecessary?
(define-pari dbltor (_fun _double -> _pointer))

(define (scm-to-gen x)
  (define (mkgenp p)
    (cpointer-push-tag! p 'GEN)
    p)
  ;; FIXME: Handle scheme bignums (integer?) correctly
  (cond [(gen-hdl? x) (gen-hdl-ref x)]
        ;; FIXME: Not sure if should make gen_0 etc. _pointers instead
        ;; of _GENs.
        [(zero? x) (gen-hdl-ref gen_0)]
        [(fixnum? x) (mkgenp (stoi x))]
        [(flonum? x) (mkgenp (dbltor x))]
        [else (error x "cannot be coverted to a GEN")]))

(define (gen-to-scm x)
  (gen-hdl x))

(define-cpointer-type _GEN #f scm-to-gen gen-to-scm)

;; Pari needs to be initialised in order to access things like avma,
;; gen_0, etc.
(pari-init-opts (expt 2 24) 0 5)

;; Constants
;; FIXME: Not sure if should make gen_0 etc. _pointers instead of _GENs.
(define-pari gen_0 _GEN)
(define-pari gen_1 _GEN)
(define-pari gen_m1 _GEN)
(define-pari gen_2 _GEN)

(define-pari GENtostr (_fun _GEN -> _string))

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

;; Documentation for GP prototypes is in Section 5.8.3.
(define return-types
  #hash((#\i . _int)
        (#\l . _long)
        (#\v . _void)
        ;; FIXME: GEN which is not gerepileupto-safe
        (#\m . _GEN)))
(define arg-types
  #hash(;; Mandatory arguments
        (#\G . _GEN)
        (#\& . 0) ; Need to "reinterpret" what this means.
        (#\L . _long)
        (#\V . 0) ; Unnecessary?
        (#\n . 0)
        (#\W . 0)
        (#\r . 0)
        (#\s . 0)
        (#\I . 0) ; These last three will have to be treated specially
        (#\E . 0) ; if we are to use them at all.
        (#\J . 0)
        ;; Automatic arguments
        (#\f . 0)
        (#\p . 0)
        (#\P . 0)
        ;; Syntax requirements
        (#\= . 0)
        ;; Optional arguments and default values
        (#\* . 0) ; Only valid after E or s
        (#\D . 0) ; Default value follows: Dvalue,type,
        ))        ; Special treatment of D when followed by G&rsVIEn

(define (gp-proto-to-func-type proto)
  (list (hash-ref return-types (string-ref proto 0))
        '->
        (map (lambda (k) (hash-ref arg-types k))
             (string->list (substring proto 1)))))
