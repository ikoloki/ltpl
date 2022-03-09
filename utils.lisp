(in-package :LTPL)

;; string Manipulation functions 

(defun string-split (string delim)
  (let ((index (search delim string)))
    (when (eq index nil)
      (return-from string-split string))
    (cons (subseq string 0 index)
          (string-split (subseq string (+ index 1) (length string)) delim))))
;; Char Utils

(defun punctp (c)
  (or (eq c #\!)
      (eq c #\?)
      (eq c #\.)
      (eq c #\,)
      (eq c #\;)))

(defun mathp (c)
  (or (eq c #\/)
      (eq c #\\)
      (eq c #\+)
      (eq c #\-)
      (eq c #\^)
      (eq c #\*)
      (eq c #\&)
      (eq c #\|)
      (eq c #\=)))
    
(defun grammarp (c)
  (let ((grammar '(#\' #\" #\~ #\` #\@)))
    (dolist (i grammar)
      (or (punctp c)
	  (eq c i)))))
      
;; data structures
(defvar *action-identifer-tree*'(()))
