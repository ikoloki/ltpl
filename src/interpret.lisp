(defpackage :ltpl
  (:use :cl)
  (:local-nicknames))

;; (defparameter +input-text* reading file in from $2)
;; (defparameter *output-text* '())

(defun start ()
  (let* (tokens (tokenize (make-string-input-stream "--$1000[p]")))
    (ast (make-object-read-stream tokens))
    ;; read Implicit tree into into symbol table then read the config file.
    (run parse)))
