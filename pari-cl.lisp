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
;; Usage: Fire up your favourite lisp then type
;;
;; * (load "pari-cl.lisp")
;; * (in-package :cl-pari)
;;


(asdf:oos 'asdf:load-op :cffi)

(defpackage :cl-pari
  (:use :common-lisp :cffi))

(in-package :cl-pari)

;; NB: ".so" is added to the file name.
;; FIXME: Work out why specifying simply "libpari" doesn't work,
;; i.e. why isn't LIBRARY_PATH used to search for libraries?
(define-foreign-library libpari
    (t (:default "/home/hlaw/local/lib/libpari")))

(use-foreign-library libpari)

;; CFFI doesn't supply a "size_t" built-in type. According to [1],
;; size_t is unsigned int or unsigned long int
;;
;; [1] https://www.gnu.org/software/libc/manual/html_node/Important-Data-Types.html
(defctype sizet :uint)

(defctype GEN (:pointer :long))
(defctype pari_sp :ulong)

(defcfun "pari_init" :void
  (stacksize sizet)
  (maxprime :ulong))

(defcfun "pari_init_opts" :void
  (stacksize sizet)
  (maxprime :ulong)
  (opts :ulong))

(defcfun "pari_close" :void)

(defcfun "pari_close_opts" :void
  (init_opts :ulong))

(defcfun "GENtostr" :string (x GEN))
(defcfun "stoi" GEN (x :long))
(defcfun "utoi" GEN (x :ulong)) ; Unnecessary?
(defcfun "dbltor" GEN (x :double))

(defcfun "gadd" GEN (x GEN) (y GEN))
(defcfun "gsub" GEN (x GEN) (y GEN))
(defcfun "gneg" GEN (x GEN))
(defcfun "gmul" GEN (x GEN) (y GEN))
(defcfun "gdiv" GEN (x GEN) (y GEN))
(defcfun "gsqr" GEN (x GEN))
(defcfun "factor" GEN (x GEN))
