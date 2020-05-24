;; -* - lisp -*-

(in-package :it.bese.qbook)

;;;; * The qbook lisp documentation system

;;;; qbook generates html formatted code listings of common lisp
;;;; source files. Comments in the source code are rendered as html
;;;; paragraphs, text is rendered in <pre> blocks. Headings are
;;;; created by preceding the text of the comment with one or more #\*
;;;; chars.

;;;; This is inspired by Luke Gorrie's pbook.el.

;;;; ** Publishing

;;;; This is the core of qbook, the driver code which takes a lisp
;;;; source file and generates an html file.

;;;; *** The public entry point: PUBLISH-QBOOK

(defclass generator ()
  ((title :accessor title :initarg :title)))

(defgeneric generate (book generator))

(defclass book ()
  ((title :accessor title :initarg :title
          :documentation "The title of the book.")
   (contents :accessor contents :initarg :contents)
   (indexes :accessor indexes :initarg :indexes :initform (make-hash-table :test 'eq))))

(defmethod book-indexes-sorted ((book book))
  (sort (hash-table-keys (indexes book))
        #'string<
        :key (lambda (descriptor-class)
               (pretty-label-prefix (make-instance descriptor-class)))))

(defmethod all-code-parts ((book book))
  (loop
    for section in (contents book)
    append (remove-if-not #'code-part-p section)))

(defmethod permutated-global-index ((book book))
  (let ((entries '()))
    (dolist (part (all-code-parts book))
      (when (descriptor part)
        (let ((name-string (symbol-name (effective-name (name (descriptor part))))))
          (dolist (name-part (remove-duplicates
                              (remove-if (lambda (string)
                                           (string= "" string))
                                         (cl-ppcre:split "[^a-zA-Z]" name-string))
                              :test #'string=))
            (let* ((offset (search name-part name-string :test #'char=))
                   (prefix (subseq name-string 0 offset))
                   (suffix (subseq name-string offset)))
              (if (symbolp (name (descriptor part)))
                  (push (list prefix suffix part)
                        entries)
                  (push (list (format nil "(~A ~A"
                                      (first (name (descriptor part)))
                                      prefix)
                              (format nil "~A)" suffix)
                              part)
                        entries)))))))
    (sort entries #'string< :key #'second)))

(defun compare-descriptor-names (a b)
  (string< (regex-replace-all "[^A-Za-z]" (string (if (symbolp (name a))
                                                      (name a)
                                                      (second (name a)))) "")
           (regex-replace-all "[^A-Za-z]" (string (if (symbolp (name b))
                                                      (name b)
                                                      (second (name b)))) "")))

(defun sort-descriptors (descriptors)
  (sort (copy-list descriptors) 'compare-descriptor-names))

(defun sort-parts-with-descriptors (parts)
  (sort (remove-if #'null parts :key #'descriptor)
        'compare-descriptor-names :key #'descriptor))

(defun convert-to-sections (file-name parts)
  (let ((sections '()))
    (iterate
      (for p in parts)
      (if (and (heading-part-p p)
               (= 1 (depth p)))
          (push (list p) sections)
          (if (consp sections)
              (push p (car sections))
              (error "No initial heading in ~S." file-name))))
    (iterate
      (for section on sections)
      (setf (car section) (nreverse (car section))))
    (nreverse sections)))

(defun bulid-indexes (book)
  (loop
    for section in (contents book)
    do (loop
         for part in section
         when (code-part-p part)
           do (when (descriptor part)
                (symbol-macrolet ((index-table
                                    (gethash (class-of (descriptor part)) (indexes book))))
                  (unless index-table
                    (setf index-table (make-hash-table :test 'eql)))
                  (setf (gethash (name (descriptor part)) index-table) part)))))
  book)

(defun publish-qbook (file-name generator)
  "Convert FILE-NAME into a qbook html file named OUTPUT-FILE with title TITLE."
  (let ((book (make-instance 'book
                             :title (title generator)
                             :contents (convert-to-sections
                                        file-name
                                        (read-source-file file-name)))))
    (bulid-indexes book)
    (generate book generator)))

;;;; ** Publishing internals

;;;; *** The classes

;;;; qbook parses lisp code into a list of source-file-part
;;;; objects. we have an object for code parts (each top level form is
;;;; considered as a single code object), for comments and for
;;;; headings.

(defclass source-file-part ()
  ((start-position :accessor start-position :initform nil :initarg :start-position)
   (end-position :accessor end-position :initform nil :initarg :end-position)
   (text :accessor text :initform nil :initarg :text)
   (origin-file :accessor origin-file :initform nil :initarg :origin-file)
   (output-file :accessor output-file :initform nil)))

(defmethod print-object ((part source-file-part) stream)
  (print-unreadable-object (part stream :type t :identity t)
    (format stream "~S" (if (< (length (text part)) 10)
                            (text part)
                            (strcat (subseq (text part) 0 10) "...")))))

(defclass code-part (source-file-part)
  ((form :accessor form :initform nil :initarg :form)
   (descriptor :accessor descriptor :initform nil :initarg :descriptor)))

(defgeneric code-part-p (object)
  (:method ((object t)) nil)
  (:method ((object code-part)) t))

(defclass comment-part (source-file-part)
  ())

(defgeneric comment-part-p (obj)
  (:method ((obj t)) nil)
  (:method ((obj comment-part)) t))

(defclass heading-part (comment-part)
  ((depth :accessor depth :initarg :depth)
   (next-part :accessor next-part :initform nil)
   (prev-part :accessor prev-part :initform nil)
   (up-part :accessor up-part :initform nil)))

(defmethod print-object ((h heading-part) stream)
  (print-unreadable-object (h stream :type t :identity nil)
    (format stream "~D ~S" (depth h) (text h))))

(defgeneric heading-part-p (obj)
  (:method ((obj t)) nil)
  (:method ((obj heading-part)) t))

(defclass whitespace-part (source-file-part)
  ())

(defmethod print-object ((part whitespace-part) stream)
  (print-unreadable-object (part stream :type t :identity t)))

;;;; *** The publishing engine

;;;; ** Directives

;;;; Directives are a way to control how qbook processes the lisp
;;;; code. We currently only support the '@include "filename"'
;;;; directive. @include allows multiple source files to be combined
;;;; to form a single html file.

(defgeneric process-directive (part))

(defmethod process-directive ((part source-file-part))
  (list part))

(defmethod process-directive ((part comment-part))
  (declare (special *source-file*))
  (multiple-value-bind (matchp strings)
      (cl-ppcre:scan-to-strings (load-time-value (cl-ppcre:create-scanner "^@include (.*)"))
                                (text part))
    (if matchp
        (return-from process-directive (read-source-file
                                        (merge-pathnames (let ((*readtable* (copy-readtable nil)))
                                                           (read-from-string (aref strings 0)))
                                                         (truename *source-file*))))
        (return-from process-directive (list part)))))

;;;; ** Parsing

;;;; A qbook source file is a lisp source file. Qbook uses the lisp's
;;;; reader to parse the code (so any valid lisp should be
;;;; usable). qbook looks for a few things in the lisp file:

;;;; 1) The code. Each top level form is wrapped in <PRE> tagged as
;;;;    pased through to the HTML. The first line (not form) of the
;;;;    top level form is presented in a bold font. If the form is
;;;;    longer than 3 lines it will be truncated to 3 lines and
;;;;    readers will have to click an the form to see the hidden text.

;;;; 2) ;;;; Comments - All lines which start with 4 #\; ("^;;;;") and
;;;;    aren't within a top level form are wrapped in a <P> tag and
;;;;    passed through.

;;;; 3) ; Comments - All comment lines with less than 4 #\; characters
;;;;    are ignored by qbook.

;;;; 4) @ directives - Lines which start with ;;;;@ are qbook
;;;;    directives. These allow the developer to control how qbook
;;;;    processes the source files. Currently the only supported
;;;;    directive is include.

;;;; A decent example of a qbook'd lisp file is qbook
;;;; itself. qbook.asd contains the include directives which control
;;;; the order of the sections while the various .lisp files contain
;;;; qbook comments, qbook headings and ignored comments (every source
;;;; file contains a copyright message which we don't want to have
;;;; repeated multiple times in the html)

;;;; *** qbook markup

;;;; There is none. You simply can't create tables or produce links or
;;;; bold text. Patches welcome.

;;;; *** The source code reader

(defun make-part-reader (function type)
  (lambda (stream echar)
    (let ((part (make-instance type)))
      (setf (start-position part) (file-position stream))
      (funcall function stream echar)
      (setf (end-position part) (file-position stream))
      part)))

(defun qbook-semicolon-reader (stream char)
  (declare (ignore char))
  (with-output-to-string (line)
    (loop
      for next-char = (read-char stream nil stream t)
      if (or (eq next-char stream)
             (char= next-char #\Newline))
        do (return)
      else
        do (write-char next-char line))))

(defun make-qbook-readtable ()
  (iterate
    (with r = (copy-readtable nil))
    (for i from 0 below 256)
    (for char = (code-char i))
    (when (get-macro-character char)
      (multiple-value-bind (function non-terminating-p)
          (get-macro-character char *readtable*)
        (set-macro-character char
                             (case char
                               (#\; (make-part-reader 'qbook-semicolon-reader 'comment-part))
                               (#\( (make-part-reader function 'code-part))
                               (t (make-part-reader function 'code-part)))
                             non-terminating-p
                             r)))
    (finally (return r))))

(defun whitespacep (char)
  (and char
       (member char '(#\Space #\Tab #\Newline) :test #'char=)))

(defun read-whitespace (stream)
  (iterate
    (with part = (make-instance 'whitespace-part))
    (initially (setf (start-position part) (1+ (file-position stream))))
    (while (whitespacep (peek-char nil stream nil nil)))
    (read-char stream)
    (finally (setf (end-position part) (file-position stream)))
    (finally (return-from read-whitespace part))))

(defun process-directives (parts)
  (iterate
    (for part in parts)
    (appending (process-directive part))))

(defun read-source-file (file-name)
  (let ((*evaling-readtable* (copy-readtable nil))
        (*evaling-package* (find-package :common-lisp-user)))
    (flet ((eval-part (part)
             (etypecase part
               (code-part
                (let* ((*readtable* *evaling-readtable*)
                       (*package* *evaling-package*)
                       (*load-pathname* (pathname file-name))
                       (*load-truename* (truename *load-pathname*)))
                  (ignore-errors
                   (setf (form part) (read-from-string (text part) nil))
                   (eval (form part)))
                  (setf *evaling-readtable* *readtable*)
                  (setf *evaling-package* *package*)))
               (t part))))
      (let* ((*readtable* (make-qbook-readtable))
             (*source-file* file-name)
             (parts (with-input-from-file (stream file-name)
                      (iterate
                        (for part in-stream stream using #'read-preserving-whitespace)
                        (collect part)
                        (when (whitespacep (peek-char nil stream nil nil))
                          (collect (read-whitespace stream)))))))
        (declare (special *source-file*))
        (with-input-from-file (stream file-name)
          (let ((buffer nil))
            (dolist (part parts)
              (file-position stream (1- (start-position part)))
              (setf buffer (make-array (1+ (- (end-position part) (start-position part)))
                                       :element-type 'character))
              (read-sequence buffer stream)
              (setf (text part) buffer
                    (origin-file part) file-name)
              (eval-part part))))
        ;; step 1: post process (merge sequential comments, setup headers, etc.)
        (setf parts (post-process parts))
        ;; step 2: handle any directives.
        (setf parts (process-directives parts))
        ;; step 3: gather any extra source code info
        (setf parts (collect-code-info parts))
        ;; step 4: setup navigation elements
        (setf parts (post-process-navigation parts))
        ;; step 5: remove all the parts before the first comment part
        (setf parts (iterate
                      (for p on parts)
                      (until (comment-part-p (first p)))
                      (finally (return p))))
        ;; done!
        parts))))

(defun heading-text-p (text)
  (scan "^;;;;\\s*\\*+" text))

(defun real-comment-p (text)
  (scan "^;;;;" text))

(defun collect-code-info (parts)
  (mapcar (lambda (part)
            (typecase part
              (code-part
               ;; punt all the work to collect-code-info
               (analyse-code-part part))
              (t part)))
          parts))

(defun post-process (parts)
  ;; convert all the comments which are acutally headings to heading
  ;; objects
  (setf parts
        (iterate
          (for p in parts)
          (typecase p
            (comment-part
             (multiple-value-bind (match strings)
                 (scan-to-strings (load-time-value
                                   (create-scanner ";;;;\\s*(\\*+)\\s*(.*)" :single-line-mode nil))
                                  (text p))
               (if match
                   (collect (make-instance 'heading-part
                                           :depth (length (aref strings 0))
                                           :text (aref strings 1)
                                           :start-position (start-position p)
                                           :end-position (end-position p)
                                           :origin-file (origin-file p)))
                   (multiple-value-bind (match strings)
                       (scan-to-strings (load-time-value
                                         (create-scanner ";;;;(.*)" :single-line-mode t))
                                        (text p))
                     (if match
                         (collect (make-instance 'comment-part
                                                 :start-position (start-position p)
                                                 :end-position (end-position p)
                                                 :text (aref strings 0)
                                                 :origin-file (origin-file p))))))))
            ((or code-part whitespace-part) (collect p)))))
  ;;;; merge consequtive comments together
  (setf parts
        (iterate
          (with comment = (make-string-output-stream))
          (for (p next) on parts)
          (cond
            ((heading-part-p p) (collect p))
            ((and (comment-part-p p)
                  (or (not (comment-part-p next))
                      (heading-part-p next)
                      (null next)))
             (write-string (text p) comment)
             (collect (make-instance 'comment-part :text (get-output-stream-string comment)))
             (setf comment (make-string-output-stream)))
            ((comment-part-p p)
             (write-string (text p) comment))
            (t (collect p)))))
  parts)

(defun post-process-navigation (parts)
    ;;;; setup the prev and next links in the header objects
  (iterate
    (with last-heading = nil)
    (for part in parts)
    (when (heading-part-p part)
      (when last-heading
        (setf (prev-part part) last-heading
              (next-part last-heading) part))
      (setf last-heading part)))
  ;;;; setup the up links
  (iterate
    (for (this . rest) on (remove-if-not #'heading-part-p parts))
    (iterate
      (for r in rest)
      (while (< (depth this) (depth r)))
      (setf (up-part r) this)))
  parts)

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
