(in-package :ltpl)

(defstruct token
  (literal)
  (symbol))

(defparameter *test* "==100,1(.*(A-Z))$0$10![w:file.txt]$foo[p]$foo$[100+1]bar.")

(defun tokenize (input-stream)
  "takes tokens from the *input-stream* and processes them util the end"
  ;; (setf src (remove-if (lambda (c) (or (eq #\SPACE c) (eq #\NEWLINE c))) src))
  (let* ((directive (tokenize-directive (with-output-to-string (tmp-stream)
                                          (loop for i from 0 to 1
                                                for c = (read-char input-stream nil) do
                                                (write-char c tmp-stream)))))
         (tokens `(,directive)))
    (loop :for c = (read-char input-stream nil)
          :while c do
            (cond ((eq c #\[) (push (tokenize-[ c) tokens))
                  ((eq c #\]) (push (tokenize-] c) tokens))
                  ((eq c #\:) (push (tokenize-colon c) tokens))
                  ((eq c #\') (push (tokenize-escape c) tokens))
                  ((eq c #\() (push (tokenize-regex input-stream) tokens))
                  ((eq c #\@) (push (tokenize-reader-end c) tokens))
                  ((eq c #\,) (push (tokenize-comma c) tokens))
                  ((eq c #\$) (push (tokenize-object input-stream c) tokens))
                  ((eq c #\.) (push (tokenize-terminator c) tokens))
                  ((operatorp c) (push (tokenize-operator c) tokens))
                  ((alphanumericp c) (push (tokenize-object input-stream c) tokens))
                  (t (push (make-token :literal c :symbol 'unknown-token) tokens))))
    (reverse tokens)))

(defun tokenize-regex (input-stream)
  "takes a regular expression out of the stream denoted by ()"
  (unread-char #\( input-stream)
  (let ((regex (with-output-to-string (output-stream)
		 (loop with paren-stack
		       for c = (read-char input-stream nil) do
			 (cond ((eq c #\() (push c paren-stack))
			       ((eq c #\)) (pop paren-stack)))
			 (write-char c output-stream)
			 (when (eq paren-stack '())
			   (loop-finish))))))
    (make-token :literal (subseq regex 1 (- (length regex) 1))
                :symbol 'regex)))


(defun tokenize-object (input-stream c)
  "takes a object referance out of the stream denoted by foo or 10 10.0 foo.0"
  (unread-char c input-stream) ;; unread c to prevent the first char from being cut off
  (let* ((object (with-output-to-string (output-stream)
                   (loop :for c := (read-char input-stream nil)
                         :for i from 0
                         :if (null c) :do
                           (loop-finish)
                         :else :if (alphanumericp c) :do
                           (write-char c output-stream)
                         :else :if (and (eq #\$ c) (= i 0)) :do
                           (write-char c output-stream)
                         :else :do
                           (unread-char c input-stream)
                           (loop-finish)))))
    (if (eq (char object 0) #\$)
        (make-token :literal object :symbol 'object-ref)
        (make-token :literal object :symbol 'object-primitive))))

(defun tokenize-directive (directive)
  (let ((mode nil))
    (cond ((equal directive "==") (setf mode 'reader))
	  ((equal directive "||") (setf mode 'reader))
	  ((equal directive "--") (setf mode 'reader))
	  ((equal directive "<=") (setf mode 'reader))
	  ((equal directive "^^") (setf mode 'reader))
    (t (setf mode 'reader-error)))
    (make-token :literal directive :symbol mode)))

(defun tokenize-operator (c)
  (make-token :literal c :symbol 'operator))

(defun tokenize-terminator (c)
  (make-token :literal c :symbol 'terminator))

(defun tokenize-escape (c)
  (make-token :literal c :symbol 'esc))

(defun tokenize-colon (c)
  (make-token :literal c :symbol 'arg-seperator))

(defun tokenize-comma (c)
  (make-token :literal c :symbol 'range-seperator))

(defun tokenize-[ (c)
  (make-token :literal c :symbol 'action-begin))

(defun tokenize-] (c)
  (make-token :literal c :symbol 'action-end))

(defun token-reader-end (c)
  (make-token :literal c :symbol 'reader))

(defun operatorp (c)
  (let ((operators '(#\\ #\+ #\- #\* #\/ #\| #\!)))
    (dolist (op operators)
      (when (eq op c)
	(return-from operatorp t)))))

