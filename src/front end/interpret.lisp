(defpackage :ltpl
  (:use :cl)
  (:local-nicknames :gen-cl :generic-cl))

;; (defparameter +input-text* reading file in from $2)
;; (defparameter *output-text* '())

(defun start ()
  (tokenize (make-string-input-stream "--$1000[p]")))
