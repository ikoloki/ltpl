(defpackage :LTPL
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)))

(defvar *tokens* '())

(defstruct token
  (literal)
  (symbol))

(defparameter *test* "==100,1(.*(A-Z))$0$10$0[w:file.txt]$foo[p]bar.")
(defparameter *input-stream* (make-string-input-stream (subseq *test* 2)))

(defun tokenize (src)
  (let* ((directive (tokenize-directive src))
	 (tokens `(,directive)))
    (loop for i from 0
	  for c = (read-char *input-stream* nil)
	  while c do
	    (cond ((eq c #\[) (push (tokenize-action) tokens))
		  ((eq c #\') (push (tokenize-escape) tokens))
		  ((eq c #\() (push (tokenize-regex) tokens))
		  ((eq c #\,) (push (tokenize-comma c) tokens))
		  ((eq c #\$) (push (tokenize-object-ref c) tokens))
		  ((eq c #\:) (push (tokenize-colon c) tokens))
		  ((eq c #\.) (push (tokenize-terminator c) tokens))
		  ((alphanumericp c) (push (tokenize-object-primitive c) tokens))
		  (t (push (make-token :literal c :symbol 'unknown-token) tokens))))
   tokens))

(defun tokenize-regex ()
  (unread-char #\( *input-stream*)
  (let ((regex (with-output-to-string (output-stream)
		 (loop with paren-stack
		       for c = (read-char *input-stream*) do
			 (cond ((eq c #\() (push c paren-stack))
			       ((eq c #\)) (pop paren-stack)))
			 (write-char c output-stream)
			 (when (eq paren-stack '())
			   (loop-finish))))))
    (make-token :literal ;;(subseq regex 1 (- (length regex) 1))
		regex :symbol 'regex)))
									    
(defun tokenize-action ()
  (let ((action (with-output-to-string (output-stream)
		 (loop with []-stack
		       for c = (read-char *input-stream*) do
			 (cond ((eq c #\[) (push c []-stack))
			       ((eq c #\]) (pop []-stack)))
			       
			       (write-char c output-stream)
			       (when (eq []-stack '())
				 (unread-char c *input-stream*)
			   (loop-finish))))))
	(make-token :literal action :symbol 'action)))

(defun tokenize-object-ref (c)
    "looks for a string with the pattern of $XXX | $XX$...$..$.. and breaks it up into 
  a token with the literal of (first (second (third)))"
  (unread-char c *input-stream*)
  (let ((object (with-output-to-string (output-stream)
		  (loop for c = (read-char *input-stream*)
			if (or (alphanumericp c)
			       (eq #\$ c)) do
				 (write-char c output-stream)
			else do 
			  (unread-char c *input-stream*)
			    (loop-finish)))))
    (make-token :literal object :symbol 'unexpanded-object)))
									    
(defun tokenize-object-primitive (c)
"looks for a alphanumeric set of chars"
  (unread-char c *input-stream*) ;; unread c to prevent the first char from being cut off
  (let ((object-primitive (with-output-to-string (output-stream)
			    (loop for c = (read-char *input-stream*)
				  if (alphanumericp c) do
				    (write-char c output-stream)
				  else do
				    (unread-char c *input-stream*)
				    (loop-finish)))))
    (make-token :literal object-primitive :symbol 'object-primitive)))

(defun tokenize-escape (c)
  (make-token :literal c :symbol 'escape-seq))

(defun tokenize-comma (c)
  (make-token :literal c :symbol 'range-seperator))

(defun tokenize-colon (c)
  (make-token :literal c :symbol 'seperator))

(defun tokenize-terminator (c)
  (make-token :literal c :symbol 'terminator))
   
(defun tokenize-directive (string)
  (let ((directive (subseq string 0 (min 2 (length string))))
	(mode nil))
    (cond ((equal directive "==") (setf mode 'normal))
	  ((equal directive "||") (setf mode 'column-normal))
	  ((equal directive "--") (setf mode 'line))
	  ((equal directive "<=") (setf mode 'reverse))
	  ((equal directive "^|") (setf mode 'column-bottom))
      	  (t (setf mode 'directive-error)))
    (make-token :literal directive :symbol mode)))
