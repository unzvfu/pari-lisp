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

(require "utils.rkt")
(require "parse-desc.rkt")

;;; What I'm calling a proto is the Racket representation of the
;;; prototype of a Pari function.
(struct proto
  (gp-name c-name param-names param-types return-type return-values)
  #:transparent)

(define *empty-proto*
  (proto "" "" '() '() #f '()))

;;; Parsing return types
(define (parse-simple-return typesym)
  (λ (p str)
    (let ([name (gensym)])
      (values (struct-copy proto p
                           [return-type `(,name : ,typesym)]
                           ;; XXX: Put this line, suitably modified,
                           ;; at the end of the code that creates the
                           ;; prototype to fix the return argument
                           ;; ordering problem.
                           [return-values
                            (cons name (proto-return-values p))])
              (substring str 1)))))

(define return-int (parse-simple-return '_int))
(define return-long (parse-simple-return '_long))
(define return-ulong (parse-simple-return '_ulong))
(define return-gen (parse-simple-return '_GEN))

(define (return-void p str)
  (values p (substring str 1)))

;;; Parsing parameters
(define (parse-simple-param typesym)
  (λ (p str)
    (let ([name (gensym)])
      (values (struct-copy proto p
                           [param-names
                            (cons name (proto-param-names p))]
                           [param-types
                            (cons `(,name : ,typesym)
                                  (proto-param-types p))])
              (substring str 1)))))

(define param-gen (parse-simple-param '_GEN))
(define param-long (parse-simple-param '_long))
(define param-ulong (parse-simple-param '_ulong))
(define param-varn (parse-simple-param '_variable))
(define param-realprec (parse-simple-param '_realprec))
(define param-seriesprec (parse-simple-param '_seriesprec))

(define (param-ref p str)
  (let ([name (gensym)])
    (values (struct-copy proto p
                         [param-names
                          (cons name (proto-param-names p))]
                         [param-types
                          (cons `(,name : (_ptr o _GEN))
                                (proto-param-types p))]
                         [return-values
                          (cons name (proto-return-values p))])
            (substring str 1))))

;; Assume for now that we always get default arguments of the form
;; "Dtype", not "Dvalue,type,"
(define (param-default p s)
 ;(let ([typ (string-ref s 1)]))
  (values p s))

(define (proto-type-signature p)
  `(_fun ,@(proto-param-names p) :: ,@(proto-param-types p)
         -> ,(proto-return-type p)
         -> (postprocess ,@(proto-return-values p))))

;; NB: Result of this function can be executed with
;; (define pari-ns (make-base-namespace)) ; Not sure this is necessary.
;; (eval (activtate-proto p) pari-ns)
(define (activate-proto p)
  `(define-pari
     ,(string->symbol (proto-gp-name p))
     ,(proto-type-signature p)
     #:c-id ,(string->symbol (proto-c-name p))))

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

;; Hash tables for the parser. Each character maps to a function
;; taking a current proto and string as input and returning the
;; updated proto and the unconsumed string.

;; NB: Can't use #hash(...) here because it QUOTEs its arguments,
;; which makes specifying λ's impossible; we need to QUASIQUOTE
;; then UNQUOTE.

(define return-types
  ;; Return types
  (make-immutable-hash
   `((#\i . ,return-int)
     (#\l . ,return-long)
     (#\u . ,return-ulong)
     (#\v . ,return-void)
     (#\m . ,return-gen))))

(define arg-types
  (make-immutable-hash
   `(;; Mandatory arguments
     (#\G . ,param-gen)
     (#\& . ,param-ref)
     (#\L . ,param-long)
     (#\U . ,param-ulong)
;;     (#\V . 0) ; Loop variable (unnecessary?)
     (#\n . ,param-varn) ; variable number
;;     (#\W . 0) ; a GEN that is an lvalue (only used for list fns)
;;     (#\r . 0) ; "raw" input
;;     (#\s . 0) ; "expanded" string
;;     (#\I . 0) ; Closure whose value is ignored (used in for loops)
;;     (#\E . 0) ; Closure whose value is used (as in sum loops)
;;     (#\J . 0) ; implicit function of one argument (as in parsum loops)
     ;; Automatic arguments
;;     (#\f . 0) ; fake long* (never used by GP)
     ; real precision
     (#\p . ,param-realprec)
     ; series precision
     (#\P . ,param-seriesprec)
     ;; Syntax requirements
;;     (#\= . 0)
     ;; Optional arguments and default values
;;     (#\* . 0) ; Only valid after E or s
     (#\D . ,param-default) ; Default value: Dvalue,type,
     )))        ; Special treatment of D when followed by G&rsVIEn

;; Given a prototype string of the form "DV,T,", returns values of a
;; ctype corresponding to T and a V of type T.
;; (define (handle-default p str)
;;   (let* ([splt (string-split pr ",")]
;;          [val (gp-read-str (substring (first splt) 1))]
;;          [typecode (hash-ref arg-types (string-ref (second splt) 0))])
;;     (values typecode
;;             ;; FIXME: This cond statement should be put somewhere more
;;             ;; accessible.  Also, need to check what other types are
;;             ;; possible.
;;             (cond [(equal? typecode '_GEN) val]
;;                   [(equal? typecode '_long) (gtolong val)]
;;                   [else (error typecode "not recognised")]))))


(define (parse-gp-proto-char tbl pr str continue [not-found (λ (p s) #f)])
  (let* ([code (string-ref str 0)]
         [parsefn (hash-ref tbl code #f)])
    (if (not parsefn)
        (not-found pr str)
        ;; Note that it would be equivalent to do
        ;;   (call-with-values (λ () (parsefn pr str)) continue)
        ;; but this inelegantly involves creating a new closure every
        ;; time. It's surprisng that call-with-values isn't a macro that
        ;; would make
        ;;   (call-with-values (parsefn pr str) continue)
        ;; expand into what I've written below.
        (let-values ([(pr str) (parsefn pr str)])
          (continue pr str)))))

(define (parse-gp-proto-param-types initpr str)
  (let loop ([pr initpr] [str str])
    (if (string-empty? str)
        pr
        (parse-gp-proto-char arg-types pr str loop))))

(define (parse-gp-proto-return-type str)
  (parse-gp-proto-char return-types *empty-proto* str values
                       (λ (p s)
                         (let-values ([(p s) (return-gen p s)])
                           (values p str)))))

;; FIXME: This makes the "actual" return value last in the list of
;; return values, whereas it should be first.
(define (parse-gp-proto str)
  (if (string-empty? str)
      ;; Special case: "" is a valid GP prototype, corresponding to a
      ;; GEN (*)(void).
      (return-gen *empty-proto* "G")
      (let-values ([(p s) (parse-gp-proto-return-type str)])
        (parse-gp-proto-param-types p (string-reverse s)))))

(define (desc-with-ch-in-proto d ch)
  (filter (λ (h)
             (let ([res (hash-ref h "Prototype" #f)])
               (and res (string-index-of res ch))))
          d))

(define (fn-with-ch d ch)
  (let ([D (desc-with-ch-in-proto d ch)])
    (map (λ (h) (hash-ref h "Function")) D)))

(define (get-fn descs nm)
  (findf (lambda (h)
           (string=? (hash-ref h "Function") nm))
         descs))

;; (define return-types "iluvm")         ; return types
;; (define can-follow-*-or-D "Es")       ; can precede a * or follow a D
;; (define can-follow-D "G&rVIn")        ; can follow a D
;; (define other-param-types "LUWJfpP=") ; everything else
