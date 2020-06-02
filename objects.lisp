;;;; cl-spotify.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


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

(in-package :cl-spotify)

(defclass spotify-object ()
  ((json :initarg :json :type st-json:jso))
  (:documentation "A Spotify object."))

(defclass spotify-track (spotify-object)
  ()
  (:documentation "A spotify track object."))

(defclass spotify-artist (spotify-object)
  ()
  (:documentation "A spotify artist object."))

(defun to-object (json)
  (if-let (obj-type (getjso "type" json))
    (let ((object-type (intern (concatenate 'string "SPOTIFY-" (string-upcase obj-type))
                               'cl-spotify)))
      (handler-case
          (when (find-class object-type)
            (make-instance object-type :json json))
        (class-not-found-error (err)
          (declare (ignorable err))
          (format t "object type ~a not implemented!~%" object-type)
          (make-instance 'spotify-object :json json))))
    (make-instance 'spotify-object :json json)))

(defmethod print-object ((object spotify-object) stream)
  (format stream "~a~%~a~%" (type-of object) (slot-value object 'json)))
