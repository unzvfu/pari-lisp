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

(define-module (math pari)
  #:use-module (system foreign)
  #:export (pari-init pari-init-opts pari-close pari-close-opts))

(define *path-to-libpari* "/home/hlaw/local/lib/libpari.so")

(define libpari (dynamic-link *path-to-libpari*))

(define pari-init
  (pointer->procedure void
                      (dynamic-func "pari_init" libpari)
                      (list size_t unsigned-long)))

(define pari-init-opts
  (pointer->procedure void
                      (dynamic-func "pari_init_opts" libpari)
                      (list size_t unsigned-long unsigned-long)))

(define pari-close
  (pointer->procedure void (dynamic-func "pari_close" libpari)))

(define pari-init-opts
  (pointer->procedure void
                      (dynamic-func "pari_close_opts" libpari)
                      (list unsigned-long)))
