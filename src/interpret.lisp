(defpackage :ltpl
  (:use :cl)
  (:local-nicknames))

;; (defparameter +input-text* reading file in from $2)
;; (defparameter *output-text* '())


(defun run ()
  (tokenize (make-string-input-stream "--$1000[p]"))
