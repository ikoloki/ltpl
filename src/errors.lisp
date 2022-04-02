(in-package :ltpl)

(defvar *parser-error-stream* (make-object-stream '()))

(defun report-errors ()
  (loop :for c = *parser-error-stream*
	:while c ;; quits on nil 
	:if (eq c)
	  
	:if 
