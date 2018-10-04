;;;; package.lisp
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :zbar-utils.test
  (:use :cl
        :fiveam
        :alexandria
        :zbar-utils))

(in-package :zbar-utils.test)

(def-suite :zbar-utils)
(in-suite :zbar-utils)

(test cv-image-scan
  (let* ((wiki-barcode (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :zbar-utils.test "wikipedia_barcode.png"))))
         (result (zbar-utils:scan-cv-image wiki-barcode)))
    (is (= 1 (length result)))
    (let ((bc (car result)))
      (is (eq (car bc) :ZBAR-EAN13))
      (is (string= (cdr bc) "5901234123457")))))

(test cv-simple-scan
  (let* ((wiki-barcode-file-name
          (format nil "~a"
                  (asdf/system:system-relative-pathname
                   :zbar-utils.test "wikipedia_barcode.png")))
         (result (zbar-utils:simple-scan wiki-barcode-file-name)))
    (is (= 1 (length result)))
    (let ((bc (car result)))
      (is (eq (car bc) :ZBAR-EAN13))
      (is (string= (cdr bc) "5901234123457")))))
