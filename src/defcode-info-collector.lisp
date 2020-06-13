;;;; ** Example of a custom code part definition

;;;; This is an example of how to define a custom code-part

(in-package :qbook)

;;;; Define a custom descriptor

(defclass info-collector-descriptor (defun-descriptor)
  ()
  (:default-initargs
   :label-prefix "code-info-collector"
   :pretty-label-prefix "Code Info Collector"))

;;;; Define an code info collector that instantiate the custom descriptor

(defcode-info-collector defcode-info-collector (name lambda-list &body body)
  (multiple-value-bind (lambda-list env)
      (arnesi::walk-lambda-list lambda-list nil nil)
    (multiple-value-bind (body docstring declarations)
        (handler-bind ((arnesi::return-from-unknown-block
                         (lambda (c)
                           (declare (ignore c))
                           (invoke-restart 'arnesi::add-block))))
          (arnesi::walk-implict-progn nil body env :docstring t :declare t))
      (declare (ignore declarations))
      (make-instance 'info-collector-descriptor
                     :name name
                     :lambda-list lambda-list
                     :body body
                     :docstring docstring))))

;;;; Finally, possibly implement custom HTML and Latex generation, specializing WRITE-CODE-DESCRIPTOR generic function.
