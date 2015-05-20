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


;; Implementation ideas:
;;
;; - All Pari constructs replaced by streams?  E.g. forprime is a
;;   stream returning prime numbers.

#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc)

;; FIXME: Clearly this should be more path-independent.
(define *path-to-libpari* "/home/hlaw/local/lib/libpari.so")

(define *pari-root* (getenv "PARI_ROOT"))

(define libpari (ffi-lib *path-to-libpari*))
(define-ffi-definer define-pari libpari)

(define-pari pari-init
  (_fun _size _ulong -> _void)
  #:c-id pari_init)

(define-pari pari-init-opts
  (_fun _size _ulong _ulong -> _void)
  #:c-id pari_init_opts)

(define-pari pari-close
  (_fun -> _void)
  #:c-id pari_close)

(define-pari pari-close-opts
  (_fun _ulong -> _void)
  #:c-id pari_close_opts)

(define (gen-print x port mode)
  (write-string (gen-to-string x) port))

(struct gen-hdl (ref [str #:auto #:mutable])
        #:auto-value #f
        ;; ref must be a GEN.
        #:guard (λ (ref type-name)
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
(define-c avma libpari _pari_sp) ; FIXME: Find out how to call this *avma*

;; Note: These types are defined in an enum at line 150 in
;; "src/headers/parigen.h".  The declaration starts:
;; /* DO NOT REORDER THESE
;;  * actual values can be changed. Adapt lontyp in gen2.c */
;; enum {
;;   t_INT    =  1,
;;   t_REAL   =  2,
;;   ...
;; };
;;
;; FIXME: It would be better to read these in each time if possible,
;; to avoid dealing with possible value changes.

;; Subtype codes needed later
(define t_VEC 17)

(define-pari cgetg (_fun _long _long -> _pointer))

(define (scmseq->genvec in)
  (let* ([lg (add1 (sequence-length in))]
         [out (cgetg lg t_VEC)])
    (for ([s (in-stream in)]
          [off (in-naturals 1)])
      (ptr-set! out _pointer off (scm->gen s)))
    out))

;; TODO: Write a number->t_INT function using integer->integer-bytes
;; to write directly into the memory of the GEN.  Would like to do the
;; same for real->floating-point-bytes, though it's less obvious how
;; to do this in high precision (i.e. might need more than 8 bytes to
;; store it).
;;(define *bytes-in-word* 4) ; FIXME
;;(define (bigint->bigint-bytes src dest)
;;  (let* ([nwords 0]
;;         [nbytes (/ nwords *bytes-in-word*)])
;;    (integer->integer-bytes src *bytes-in-word*
;;                            (system-big-endian?) dest)))

;;(define (scmint->genint x)
;;  (let ([z (cgetg lg t_INT)])
;;    (bigint->bigint-bytes x (off1 (gen-hdl-ref z)))
;;    z))

(define (scm->gen x)
  (let ([mkgenp (λ (p)
                  (cpointer-push-tag! p 'GEN)
                  p)])
    (cond [(gen-hdl? x) (gen-hdl-ref x)]
          ;; TODO: See if it's faster to use (number->string x r) for
          ;; radix r = 16, 32 or 2^64 for example.
          [(number? x) (mkgenp (gp-read-str (number->string x)))]
          [(sequence? x) (mkgenp (scmseq->genvec x))]
          [else (error x "cannot be coverted to a GEN")])))

(define (gen->scm x)
  (gen-hdl x))

(define-cpointer-type _GEN #f scm->gen gen->scm)

(define-pari GENtostr (_fun _GEN -> _string))
(define-pari gp-read-str (_fun _string -> _pointer)
  #:c-id gp_read_str)

;; FIXME: Use getrealprecision()?
(define *real-prec* 3)
(define *series-prec* 3)

(define-pari gclone (_fun _GEN -> _GEN))

;; Pari needs to be initialised in order to access things like avma,
;; gen_0, etc.
(define *default-stack-size* (expt 2 24))  ; 16MiB
(define *default-prime-limit* 0)  ; Default default
(define *signal-options* 5) ; FIXME: Document why this is necessary
(define *orig-avma* 0)

(define (pari-start-session)
  (pari-init-opts *default-stack-size*
                  *default-prime-limit*
                  *signal-options*)
  (set! *orig-avma* avma))

(define (pari-end-session)
  (pari-close-opts *signal-options*))

(define-fun-syntax _realprec
  (syntax-id-rules (_realprec)
    [_realprec (type: _long expr: *real-prec*)]))

(define-fun-syntax _seriesprec
  (syntax-id-rules (_seriesprec)
    [_seriesprec (type: _long expr: *series-prec*)]))

(define (postprocess args)
  (let ([clones (map gclone args)])
    (set! avma *orig-avma*)
    (apply values clones)))

(define-pari ellfromj
  (_fun _GEN
        -> (res : _GEN)
        -> (postprocess (list res)))
  #:c-id ellfromj)

(define-pari sqrtn
  (_fun _GEN _GEN (zetan : (_ptr o _GEN)) _realprec
        -> (res : _GEN)
        -> (postprocess (list res zetan)))
  #:c-id gsqrtn)

;; What is the difference between make-ctype, _pointer, _cpointer and
;; define-cpointer-type?
;;
;; - define-cpointer-type is a macro version of _cpointer, which
;; provides a predicate and names the tag properly.
;;
(define _GEN-or-null
  (make-ctype
   _GEN
   (lambda (v) (and v (gen->scm v)))
   (lambda (v) (and v (scm->gen v)))))

;; FIXME: For some reason the _or-null business prevents scm->gen
;; from being called, so for example (gcd 12 4) gives an error saying
;; that 4 is not a the correct type.  It works if one converts 4 to a
;; GEN manually: (gcd 12 (scm->gen 4)) ~> 4.  It always works as
;; expected when the second argument is omitted (so takes its default
;; value #f, hence passes NULL to Pari).
(define-pari gcd
  ;; Type code GDG
  (_fun (x [y #f]) :: (x : _GEN) (y : _GEN-or-null) ; (y : (_or-null _GEN))
        -> (res : _GEN)
        -> (postprocess (list res)))
  #:c-id ggcd0)

(define-pari type
  (_fun _GEN -> _GEN)
  #:c-id type0)

(define-pari fetch-user-var
  (_fun _string -> _long)
  #:c-id fetch_user_var)

(define (symbol->varn s)
  (fetch-user-var (symbol->string s)))

(define-fun-syntax _variable
  (syntax-id-rules (_variable)
    [_variable (bind: s type: _long pre: (symbol->varn s))]))

(define-pari Pol
  (_fun (t [v 'x]) :: (t : _GEN) (v : _variable)
        -> (res : _GEN)
        -> (postprocess (list res)))
  #:c-id gtopoly)

(define-pari subst
  (_fun (x y z) :: (x : _GEN) (y : _variable) (z : _GEN)
        -> (res : _GEN)
        -> (postprocess (list res)))
  #:c-id gsubst)
