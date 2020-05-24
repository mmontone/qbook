;; -*- lisp -*-

(in-package :it.bese.qbook)

;;;; * The HTML Generator

(defclass html-generator (generator)
  ((escape-comments :initarg :escape-comments
                    :accessor escape-comments
                    :type boolean
                    :initform t
                    :documentation "If T, escape comments HTML. If NIL, output the comment as it is (useful for embedding HTML in code comments).")
   (output-directory :initarg :output-directory :accessor output-directory)))

(defvar *generator*)

(defvar *book*)

(defmethod generate (book (generator html-generator))
  (let ((*generator* generator)
        (*book* book))
    (let ((output-dir-truename (ensure-directories-exist
                                (merge-pathnames (output-directory generator)))))
      (write-string-to-file *print.css* (make-pathname :name "print" :type "css"
                                                       :defaults output-dir-truename)
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (write-string-to-file *style.css* (make-pathname :name "style" :type "css"
                                                       :defaults output-dir-truename)
                            :if-does-not-exist :create
                            :if-exists :supersede))
    (generate-table-of-contents (contents book) generator)
    (dolist (section (contents book))
      (generate-section section generator))
    (dolist (index-class (book-indexes-sorted book))
      (generate-index generator book index-class))
    (generate-permuted-index generator book)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (yaclml:deftag-macro <qbook-page (&attribute title file-name (stylesheet "style.css")
                                               (printsheet "print.css")
                                               &body body)
    `(with-output-to-file (*yaclml-stream*
                           (ensure-directories-exist (merge-pathnames ,file-name (output-directory *generator*)))
                           :if-exists :supersede
                           :if-does-not-exist :create)
       (<:html
        (<:head
         (<:title (<:as-html ,title))
         (<:stylesheet ,stylesheet)
         (<:link :rel "alternate stylesheet" :href ,printsheet :title "Print"))
        (<:body
         (<:div :class "qbook" ,@body))))))

(defun generate-table-of-contents (sections generator)
  (<qbook-page :title (title generator)
               :file-name "index.html"
               (<:div :class "contents"
                      (<:h1 :class "title" (<:as-html (title generator)))
                      (<:h2 "Table of Contents")
                      (dolist (section sections)
                        (dolist (part section)
                          (when (heading-part-p part)
                            (<:div :class (strcat "contents-heading-" (depth part))
                                   (<:a :href (make-anchor-link part)
                                        (<:as-html (text part)))))))
                      (<:h2 "Indexes")
                      (dolist (index (book-indexes-sorted *book*))
                        (<:div :class "contents-heading-1"
                               (<:a :href (strcat "index/" (label-prefix (make-instance index)) ".html")
                                    (<:as-html (pretty-label-prefix (make-instance index)))
                                    " Index")))
                      (<:div :class "contents-heading-1"
                             (<:a :href "index/permutated.html" "Permuted Symbol Index")))))

(defun generate-index (generator book index-class)
  (declare (ignore generator))
  (<qbook-page :title (strcat (pretty-label-prefix (make-instance index-class))
                              " Index")
               :file-name (strcat "index/" (label-prefix (make-instance index-class)) ".html")
               :stylesheet "../style.css"
               :printsheet "../print.css"
               (<:div :class "api-index"
                      (<:h1 (<:as-html (strcat (pretty-label-prefix (make-instance index-class))
                                               " Index")))
                      (<:div :class "contents"
                             (<:dl
                              (dolist (part (sort-parts-with-descriptors (hash-table-values (gethash index-class (indexes book)))))
                                (<:dt (<:a :href (strcat "../" (make-anchor-link (descriptor part)))
                                           (<:as-html (name (descriptor part)))))
                                (when (docstring (descriptor part))
                                  (<:dd (<:as-html (docstring-first-sentence (descriptor part))))))))))
  t)

(defun generate-permuted-index (generator book)
  (declare (ignore generator))
  (<qbook-page :title "Permuted Index"
               :file-name "index/permutated.html"
               :stylesheet "../style.css"
               :printsheet "../print.css"
               (<:div :class "api-index"
                      (<:h1 (<:as-html "Permuted Index"))
                      (<:div :class "contents"
                             (<:table :class "permuted-index-table"
                                      (dolist* ((prefix suffix part) (permutated-global-index book))
                                        (<:tr
                                         (<:td :align "right"
                                               (<:a :href (strcat "../" (make-anchor-link (descriptor part)))
                                                    (<:as-html prefix)))
                                         (<:td (<:a :href (strcat "../" (make-anchor-link (descriptor part)))
                                                    (<:as-html suffix)))
                                         (<:td (<:a :href (strcat "../" (make-anchor-link (descriptor part)))
                                                    " [" (<:as-html (pretty-label-prefix (descriptor part))) "] ")))))))))

(defun generate-section (section generator)
  (<qbook-page :title (title generator)
               :file-name (make-pathname :name (make-anchor-name (text (first section)))
                                         :type "html")
               (output-directory generator)
               (<:h1 :class "title" (<:as-html (title generator)))
               (<:div :class "contents"
                      (publish section))))

(defmethod make-anchor-link ((h heading-part))
  (if (= 1 (depth h))
      (strcat (make-anchor-name (text h)) ".html")
      (labels ((find-level-1 (h)
                 (if (= 1 (depth h))
                     h
                     (find-level-1 (up-part h)))))
        (strcat (make-anchor-link (find-level-1 h)) "#" (make-anchor-name (text h))))))

(defmethod make-anchor-link ((d descriptor))
  (if (name d)
      (concatenate 'string "api/" (make-anchor-name d) ".html")
      "#"))

(defmethod make-anchor-name ((text string))
  (regex-replace-all "[^A-Za-z.-]" text
                     (lambda (target-string start end match-start match-end reg-starts reg-ends)
                       (declare (ignore start end match-end reg-starts reg-ends))
                       (format nil "_~4,'0X" (char-code (aref target-string match-start))))))

(defun effective-name (function-name)
  (if (symbolp function-name)
      function-name
      (second function-name)))

(defmethod make-anchor-name ((descriptor descriptor))
  (make-anchor-name (strcat (label-prefix descriptor)
                            "_"
                            (package-name (symbol-package (effective-name (name descriptor))))
                            "::"
                            (if (symbolp (name descriptor))
                                (symbol-name (name descriptor))
                                (format nil "(~A ~A)"
                                        (symbol-name (first (name descriptor)))
                                        (symbol-name (second (name descriptor))))))))

(defmethod make-anchor-name ((method-descriptor defmethod-descriptor))
  (make-anchor-name (strcat (label-prefix method-descriptor)
                            "_"
                            (package-name (symbol-package (effective-name (name method-descriptor))))
                            "::"
                            (html-name method-descriptor))))

(defmethod html-name ((descriptor descriptor))
  (name descriptor))

(defmethod html-name ((descriptor defmethod-descriptor))
  (format nil "(~A~@[ ~A~]~{ ~A~})"
          (name descriptor)
          (qualifier descriptor)
          (remove-if #'null
                     (mapcar (lambda (argument)
                               (typecase argument
                                 (arnesi::specialized-function-argument-form
                                  (arnesi::specializer argument))))
                             (lambda-list descriptor)))))

(defun publish (parts)
  (iterate
    (with state = nil)
    (for p in parts)
    (setf (output-file p) (strcat (make-anchor-name (text (first parts))) ".html"))
    (etypecase p
      (comment-part (setf state (write-comment p state)))
      (whitespace-part (setf state nil) (<:as-html (text p)))
      (code-part (setf state (write-code p state))))))

(defun num-lines (text)
  (iterate
    (with num-lines = 0)
    (for char in-string text)
    (when (member char '(#\Newline #\Return #\Linefeed))
      (incf num-lines))
    (finally (return num-lines))))

(defun write-code (part state)
  (ecase state
    ((nil) nil)
    (:in-comment
     (setf state nil)
     (write-string "</p>" *yaclml-stream*)
     (terpri *yaclml-stream*)))
  (write-code-descriptor (descriptor part) part)
  nil)

(defgeneric write-code-descriptor (descriptor part))

(defmethod write-code-descriptor ((descriptor t) part)
  (let ((text (text part)))
    (setf text (yaclml::escape-as-html text))
    (setf text (regex-replace-all "(\\(|\\))"
                                  text
                                  "<span class=\"paren\">\\1</span>"))
    (setf text (regex-replace "^.*"
                              text
                              (strcat "<span class=\"first-line\">\\&</span><span class\"body\">")))
    (<:pre :class "code" (<:as-is text) (<:as-is "</span>"))))

(defmethod write-code-descriptor :around ((descriptor descriptor) part)
  (<:div :class (strcat "computational-element-link "
                        "computational-element-link-" (label-prefix descriptor))
         (<:p (<:a :name (make-anchor-name descriptor)
                   :href (make-anchor-link descriptor)
                   (<:as-html (pretty-label-prefix descriptor))
                   " "
                   (<:as-html (html-name descriptor)))
              " "
              (when-bind first-sentence (docstring-first-sentence descriptor)
                (<:as-html first-sentence))))
  (<qbook-page :title (strcat (pretty-label-prefix descriptor) " " (html-name descriptor))
               :file-name (make-anchor-link descriptor)
               :stylesheet "../style.css"
               :printsheet "../print.css"
               (<:div :class "computational-element"
                      (<:h1 (<:as-html (pretty-label-prefix descriptor)) ": " (<:as-html (html-name descriptor)))
                      (<:div :class "contents"
                             (when (docstring descriptor)
                               (<:h2 "Documentation")
                               (<:blockquote
                                (<:as-html (docstring descriptor))))
                             (call-next-method)
                             (<:h2 "Source")
                             (<:pre :class "code" (<:as-html (text part)))
                             (<:a :href (strcat "../" (output-file part) "#" (make-anchor-name (descriptor part)))
                                  "Source Context")))))

(defmethod write-code-descriptor ((descriptor descriptor) part)
  (declare (ignore part))
  nil)

(defmethod write-code-descriptor ((descriptor defclass-descriptor) part)
  (declare (ignore part))
  (when (slots descriptor)
    (<:h2 "Slots")
    (<:ul
     (dolist (slot (slots descriptor))
       (<:li (<:as-html (name slot))
             (when (docstring slot)
               (<:as-html " - " (docstring slot)))))))
  (<:h2 "Hierachy")
  (<:h3 "Precedence List")
  (flet ((make-class-link (class)
           (aif (find-descriptor "class" (class-name class))
                (<:a :href (strcat "../" (make-anchor-link it))
                     (<:as-html (class-name class)))
                (<:as-html (class-name class)))))
    (<:ul
     (dolist (class (mopp:class-direct-superclasses (find-class (name descriptor))))
       (<:li (make-class-link class))))
    (awhen (mopp:class-direct-subclasses (find-class (name descriptor)))
      (<:h3 "Sub Classes")
      (<:ul
       (dolist (sub it)
         (<:li (make-class-link sub)))))))

;;;; ** Writing Comments

(defun write-comment (part state)
  (etypecase part
    (heading-part
     (ecase state
       ((nil))
       (:in-comment
        ;; heading during a comment, break the current comment
        ;; and start a new one.
        (write-string "</p>" *yaclml-stream*)
        (terpri *yaclml-stream*)))
     (flet ((heading ()
              (<:a :name (make-anchor-name (text part)) (<:as-html (text part)))
              (<:as-is "&nbsp;"))
            (nav-links ()
              (<:div :class "nav-links"
                     (if (prev-part part)
                         (<:a :class "nav-link" :href (make-anchor-link (prev-part part)) "prev")
                         (<:span :class "dead-nav-link" "prev"))
                     " | "
                     (if (up-part part)
                         (<:a :class "nav-link" :href (make-anchor-link (up-part part)) "up")
                         (<:span :class "dead-nav-link" "up"))
                     " | "
                     (if (next-part part)
                         (<:a :href (make-anchor-link (next-part part)) "next")
                         (<:span :class "nav-link" "next"))
                     " | "
                     (<:a :href "index.html" "toc"))))
       (case (depth part)
         (1 (<:h2 (heading)))
         (2 (<:h3 (heading)))
         (3 (<:h4 (heading)))
         (4 (<:h5 (heading)))
         (5 (<:h6 (heading)))
         (t (error "Nesting too deep: ~S." (text part))))
       (nav-links))
     nil)
    (comment-part
        ;;;; regular comment
     (ecase state
       ((nil) (write-string "<p>" *yaclml-stream*))
       (:in-comment nil))
     (if (escape-comments *generator*)
         (<:as-html (text part))
         (<:as-is (text part)))
     :in-comment)))

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
