#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define *path-to-libpari* "/home/hlaw/local/lib/libpari.so")

(define-ffi-definer define-pari (ffi-lib *path-to-libpari*))

(define-cpointer-type _GEN #f
  #f ; FIXME: Add this
  (lambda (x)  ; FIXME: Use OUTPUT instead
    (error '_GEN "cannot be used as an output type")))

;; NB: For some reason Pari initialisation goes awry and Racket
;; segfaults if INIT_SIGm is enabled in PARI-INIT-OPTS, which it is by
;; default in PARI-INIT.  So to initialise Pari one runs
;;
;;   (pari-init-opts *stack-size* *maxprime* 5)
;;
;; since 5 == INIT_DFTm | INIT_JMPm, which are the default options
;; less INIT_SIGm.

;; NB: If PARI-CLOSE is called, then the next command, including
;; exiting immediately afterwards (with CTRL-d for example), causes
;; Racket to segfault.  This happens whether or not INIT_JMPm is
;; activated.  It does NOT happen if we initialise Pari and then exit
;; from Racket without calling PARI-CLOSE.

(define-pari pari-init (_fun _size _ulong -> _void)
  #:c-id pari_init)
(define-pari pari-init-opts (_fun _size _ulong _ulong -> _void)
  #:c-id pari_init_opts)
(define-pari pari-close (_fun -> _void)
  #:c-id pari_close)
(define-pari stoi (_fun _long -> _GEN))
(define-pari utoi (_fun _ulong -> _GEN))
(define-pari factor (_fun _GEN -> _GEN))
(define-pari output (_fun _GEN -> _void))


(define-pari gadd (_fun _GEN _GEN -> _GEN))
(define-pari gsub (_fun _GEN _GEN -> _GEN))
(define-pari gneg (_fun _GEN -> _GEN))
(define-pari gmul (_fun _GEN _GEN -> _GEN))
(define-pari gdiv (_fun _GEN _GEN -> _GEN))
(define-pari gsqr (_fun _GEN -> _GEN))


;; (pari-init-opts 16000000 0 5)
;; (define a (utoi 8))
;; (define b (stoi -9))
;; (define c (gmul a (gsqr b)))
;; (define f (factor c))
