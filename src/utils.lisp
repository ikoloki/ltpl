(in-package :LTPL)

;; string Manipulation functions 
(defun string-split (string delim)
  (let ((index (search delim string)))
    (cons (subseq string 0 index)
          (when index
            (string-split (subseq string (+ index 1)) delim)))))

(defun string-wordp (string)
  (let ((matches '()))
    (loop for c across string do
      (when (alpha-charp c)
	(push t matches)))
    (= (length matches)
       (length string))))

(defun string-numberp (string)
    (let ((matches '()))
    (loop for c across string do
      (when (digit-char-p c)
	(push t matches)))
    (= (length matches)
       (length string))))
