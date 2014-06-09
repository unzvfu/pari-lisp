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
(require "utils.rkt")

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
(define-pari gtos (_fun _pointer -> _long))
(define-pari gtodouble (_fun _pointer -> _double))

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
(define-pari gp-read-str (_fun _string -> _pointer)
  #:c-id gp_read_str)

(define _realprec _long)
(define _seriesprec _long)
(define-pari gadd (_fun _GEN _GEN -> _GEN))
(define-pari gsub (_fun _GEN _GEN -> _GEN))
(define-pari gneg (_fun _GEN -> _GEN))
(define-pari gmul (_fun _GEN _GEN -> _GEN))
(define-pari gdiv (_fun _GEN _GEN -> _GEN))
(define-pari gsqr (_fun _GEN -> _GEN))
(define-pari factor (_fun _GEN -> _GEN))
(define-pari gsqrtn (_fun _GEN _GEN (zetan : (_ptr o _GEN)) _realprec
                          -> (res : _GEN)
                          -> (values res zetan)))

;; long u_lvalrem(ulong x, ulong y, ulong *r) "lUUU&":
(define-pari u_lvalrem (_fun _ulong _ulong (r : (_ptr o _ulong))
                             -> (res : _long)
                             -> (values res r)))

;; Some stack management functions:
(define-pari gcopy (_fun _GEN -> _GEN))
(define-pari gerepilecopy (_fun _pari_sp _GEN -> _GEN))
(define-pari gerepileupto (_fun _pari_sp _GEN -> _GEN))

;; GEN characteristic(GEN x) "mG":
;; The 'm' means the function is 'shallow' in some sense; this is
;; resolved by simply copying the result.
(define-pari characteristic/raw (_fun _GEN -> _GEN)
  #:c-id characteristic)
(define (characteristic x) (gcopy (characteristic/raw x)))

;; GEN member_j(GEN x) "mG" (the .j member function):
(define-pari .j/raw (_fun _GEN -> _GEN)
  #:c-id member_j)
(define (.j x) (gcopy (.j/raw x)))

;; Elliptic curves
(define-pari ellfromj (_fun _GEN -> _GEN))
;; FIXME: Need more general ellinit eventually.
(define-pari ellinit (_fun _GEN (_GEN = gen_1) (_realprec = 3) -> _GEN))


;; void untilpari(GEN E, GEN I) "vEI"
;; N.B. We need to build the "entree*" from the given function
;; pointer, which is then given to untilpari().
(define-pari until (_fun (_fun -> _GEN) (_fun -> _void)
                         -> _void)
  #:c-id untilpari)
;; void forprime(...) "vV=GDGI":
;; We really want this to be used like so:
;; (forprime [p 2 1000]
;;   (let ([q (* p p)])
;;     (display q)))
;; in which the second argument is a progn.
;; NB: Here the third parameter (the second GEN) is optional, but it
;; is followed by a non-optional parameter with no default, so we will
;; need to transform this somehow, since once a parameter has been
;; made optional, all following parameters must also have defaults.
;; Maybe the 'case-lambda' construction will be useful here?
;(define-pari forprime (_fun _variable _GEN _GEN (_fun -> _void)
;                       -> void))


(define (+ . args)
  (foldl gadd gen_0 args))

(define (* . args)
  (foldl gmul gen_1 args))

;; We need to use these prototype codes to build wrappers for calls to
;; libpari functions.  For example, given "mDx,G," for a function "F"
;; we need to
;; - produce the function signature (_fun _GEN -> _GEN) for F,
;; - call (define-pari F/raw signature #:c-id F)
;; - write a wrapper for F which
;;   - sets the argument to x if not specified,
;;   - calls F/raw
;;   - calls gcopy on the result (because of the "m")

;; Documentation for GP prototypes is in Section 5.8.3.
(define return-types
  #hash((#\i . _int)
        (#\l . _long)
        (#\u . _ulong)
        (#\v . _void)
        ;; FIXME: GEN which is not gerepileupto-safe
        (#\m . _GEN)))

;; Each character maps to a function taking a string as input and
;; returning the argument type and the unconsumed string.
(define arg-types
  ;; NB: Can't use #hash(...) here because it QUOTEs its arguments,
  ;; which makes specifying lambda's impossible; we need to QUASIQUOTE
  ;; then UNQUOTE.
  (make-immutable-hash
   `(;; Mandatory arguments
     (#\G . ,(lambda (str) (values '_GEN (substring str 1))))
     ; Should convert to a function that returns multiple values.
     ; When the reference is optional (i.e. the code is "D&"), we
     ; should pass a flag regarding whether or not the user wishes to
     ; calculate it; or we could just *always* calculate it.
     (#\& . 0) ; GEN*
     (#\L . ,(lambda (str) (values '_long (substring str 1))))
     (#\U . ,(lambda (str) (values '_ulong (substring str 1))))
     (#\V . 0) ; Loop variable (unnecessary?)
     (#\n . 0) ; variable number
     (#\W . 0) ; a GEN that is an lvalue
     (#\r . 0) ; "raw" input
     (#\s . 0) ; "expanded" string
     ;; Need to build closures from Racket functions.  Do this with
     ;; code such as
     ;;
     ;; (define (scm-fn x) (* x (+ 7 x)))
     ;; (define c-ptr-to-scm-fn
     ;;   (function-ptr scm-fn (_fun _long -> _long)))
     ;; (define-blah capply (_fun _pointer _long -> _long))
     ;; (capply c-ptr-to-scm-fn 3) ; OUT> 30
     ;;
     ;; Can create a closure with snm_closure() in libpari, which
     ;; takes an entree* and a t_VEC of data.  An entree* contains
     ;; just plain old data, including a function pointer to the code
     ;; to call.
     (#\I . 0) ; Closure whose value is ignored (used in for loops)
     (#\E . 0) ; Closure whose value is used (as in sum loops)
     (#\J . 0) ; implicit function of one argument (as in parsum loops)
     ;; Automatic arguments
     (#\f . 0) ; fake long* (i.e. unused return-value parameter)
     (#\p . 0) ; real precision
     (#\P . 0) ; series precision
     ;; Syntax requirements
     (#\= . 0)
     ;; Optional arguments and default values
     (#\* . 0) ; Only valid after E or s
     (#\D . 0) ;,handle-default) ; Default value follows: Dvalue,type,
     )))        ; Special treatment of D when followed by G&rsVIEn

;; Given a prototype string of the form "DV,T,", returns values of a
;; ctype corresponding to T and a V of type T.
(define (handle-default proto)
  (let* ([splt (string-split proto ",")]
         [val (gp-read-str (substring (first splt) 1))]
         [typecode (hash-ref arg-types (string-ref (second splt) 0))])
    (values typecode
            ;; FIXME: This cond statement should be put somewhere more
            ;; accessible.  Also, need to check what other types are
            ;; possible.
            (cond [(equal? typecode '_GEN) val]
                  [(equal? typecode '_long) (gtos val)]
                  [else (error typecode "not recognised")]))))

(define (parse-gp-proto proto)
  (reverse
   (let loop ([acc '()] [proto proto])
     (if (= (string-length proto) 0)
         acc
         (let* ([code (string-ref proto 0)]
                [parsefn (hash-ref arg-types code)])
           (let-values ([(arg rest) (parsefn proto)])
             (loop (cons arg acc) rest)))))))


(define (gp-proto-to-func-type proto)
  (let* ([rtn (hash-ref return-types (string-ref proto 0) #f)]
         [args (map (lambda (k) (hash-ref arg-types k))
                    (string->list (substring proto (if rtn 1 0))))])
    (list* (if rtn rtn '_GEN) '-> args)))
