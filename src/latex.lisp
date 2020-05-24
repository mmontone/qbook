;; -*- lisp -*-

(in-package :it.bese.qbook)

;;;; * The LaTeX Generator

(defclass latex-generator (generator)
  ((output-file :initarg :output-file :accessor output-file)
   (listings :initarg :listings :accessor listings :initform nil
             :documentation "When non-NIL, generate listings with LaTeX listings package.  When string, push \\ltset{this string} in the preamble.")))

(defvar *latex-stream*)

(defun \\command (name &rest args)
  (declare (special *latex-stream*))
  (write-string "\\" *latex-stream*)
  (write-string name *latex-stream*)
  (dolist (arg args)
    (write-string "{" *latex-stream*)
    (write-string arg *latex-stream*)
    (write-string "}" *latex-stream*))
  (terpri *latex-stream*))

(defgeneric generate-part (part generator))

(defmethod generate (book (generator latex-generator))
  (with-output-to-file (*latex-stream* (output-file generator)
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
    (declare (special *latex-stream*))
    (write-line "\\documentclass[a4paper]{article}" *latex-stream*)
    (when (listings generator)
      (\\command "usepackage" "listings")
      (\\command "lstset" "language=lisp")
      (when (stringp (listings generator))
        (\\command "lstset" (listings generator))))
    (\\command "title" (title generator))
    (\\command "date" "")
    (\\command "begin" "document")
    (\\command "maketitle")
    (\\command "tableofcontents")
    (dolist (section (contents book))
      (dolist (part section)
        (generate-part part generator)))
    (\\command "end" "document")))

(defmethod generate-part ((part code-part) (generator latex-generator))
  (\\command "begin" (if (listings generator) "lstlisting" "verbatim"))
  (write-string (text part) *latex-stream*)
  (terpri *latex-stream*)
  (\\command "end" (if (listings generator) "lstlisting" "verbatim")))

(defmethod generate-part ((part whitespace-part) (generator latex-generator))
  (write-string (text part) *latex-stream*))

(defmethod generate-part ((part heading-part) (generator latex-generator))
  (write-string (ecase (depth part)
                  (1 "\\section{")
                  (2 "\\subsection{")
                  (3 "\\subsubsection{")
                  (4 "\\subsubsection*{"))
                *latex-stream*)
  (write-latex-escaped (text part) *latex-stream*)
  (write-string "}" *latex-stream*)
  (terpri *latex-stream*))

(defmethod generate-part ((part comment-part) (generator latex-generator))
  (write-latex-escaped (text part) *latex-stream*))

(defun write-latex-escaped (string stream)
  (iterate
    (for char in-string string)
    (case char
      ((#\& #\$ #\% #\# #\_ #\{ #\} #\^)
       (write-char #\\ stream)
       (write-char char stream)
       (write-string "{}" stream))
      (#\\ (write-string "$\\backslash$" stream))
      (t (write-char char stream)))))

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
