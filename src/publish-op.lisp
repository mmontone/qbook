;; -*- lisp -*-

(in-package :it.bese.qbook)

;;;; * ASDF Integration

;;;; The publish-op generates documentation from the ASDF system
;;;; definition. The op creates a qbook html file next to the .asd
;;;; file. The default values for the parameters passed to
;;;; PUBLISH-QBOOK (input-file, output-file and title) are all taken
;;;; from the ASDF system. Customizing the defaults is a simple matter
;;;; of passing the proper keywords to asdf:oos.

(defclass publish-op (asdf:operation)
  ((generator :initarg :generator :accessor generator)
   (input-file :initform nil :initarg :input-file :accessor input-file)))

(defmethod input-files ((op publish-op) (system asdf:system))
  (let ((x (or (input-file op) (asdf:system-source-file system))))
    (and x (list x))))

(defmethod asdf:perform ((op publish-op) (system asdf:system))
  (publish-qbook (first (input-files op system)) (generator op)))

(defmethod asdf:perform ((op publish-op) (component t))
  t)

(defmethod asdf:operation-done-p ((op publish-op) (component t))
  nil)

;; Copyright (c) 2005, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
