(defpackage :LTPL
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)))

;; (defparameter +input-text* reading file in from $2)
;; (defparameter *output-text* '())

(defparameter +input-stream+ (make-string-input-stream (subseq *source* 2)))
(defparameter +tokens+ (tokenize source))
(defparameter +ast+ (parse +tokens+))
;; (interpret +ast+)
