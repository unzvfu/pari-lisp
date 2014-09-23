;; Constants
;; FIXME: Not sure if should make gen_0 etc. _pointers instead of _GENs.
(define-pari gen_0 _GEN)
(define-pari gen_1 _GEN)
(define-pari gen_m1 _GEN)
(define-pari gen_2 _GEN)



(define-pari gadd (_fun _GEN _GEN -> _GEN))
(define-pari gsub (_fun _GEN _GEN -> _GEN))
(define-pari gneg (_fun _GEN -> _GEN))
(define-pari gmul (_fun _GEN _GEN -> _GEN))
(define-pari gdiv (_fun _GEN _GEN -> _GEN))
(define-pari gsqr (_fun _GEN -> _GEN))
(define-pari factor (_fun _GEN -> _GEN))
;; GEN gsqrtn(GEN x, GEN n, GEN *z = NULL, long prec) "GGD&p"
(define-pari gsqrtn (_fun _GEN _GEN (zetan : (_ptr o _GEN)) _realprec
                          -> (res : _GEN)
                          -> (values res zetan)))


;; GEN characteristic(GEN x) "mG":
;; The 'm' means the function is 'shallow' in some sense; this is
;; resolved by simply copying the result.
(define-pari characteristic (_fun _GEN -> (r : _GEN) -> (gcopy r)))

;; GEN member_j(GEN x) "mG" (the .j member function):
(define-pari .j (_fun _GEN -> (r : _GEN) -> (gcopy r))
  #:c-id member_j)

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
;;     (displayln q)))
;; in which the second argument is a progn.
;; OR, like so
;; (for/list ([p (in-stream (prime-stream 2 1000))])
;;   (let ([q (* p p)])
;;     (displayln q)))
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
