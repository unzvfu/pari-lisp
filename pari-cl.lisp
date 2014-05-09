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


(asdf:oos 'asdf:load-op :cffi)

(defpackage :cl-pari
  (:use :common-lisp :cffi))

(in-package :cl-pari)

;; NB: ".so" is added to the file name.
(define-foreign-library libpari
    (t (:default "/home/hlaw/local/lib/libpari")))

(use-foreign-library libpari)

;; CFFI doesn't supply a "size_t" built-in type. According to [1],
;; size_t is unsigned int or unsigned long int
;;
;; [1] https://www.gnu.org/software/libc/manual/html_node/Important-Data-Types.html
(defctype sizet :uint)

(defctype GEN (:pointer :long))

(defcfun "pari_init" :void
  (stacksize sizet)
  (maxprime :ulong))

(defcfun "pari_init_opts" :void
  (stacksize sizet)
  (maxprime :ulong)
  (flags :long))

(defcfun "pari_close" :void)

(defcfun "stoi" GEN (x :long))
(defcfun "utoi" GEN (x :ulong))
(defcfun "factor" GEN (x GEN))
(defcfun "gadd" GEN (x GEN) (y GEN))
(defcfun "sub" GEN (x GEN) (y GEN))
(defcfun "gneg" GEN (x GEN))
(defcfun "gmul" GEN (x GEN) (y GEN))
(defcfun "gdiv" GEN (x GEN) (y GEN))
(defcfun "gsqr" GEN (x GEN))
