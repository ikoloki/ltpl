(in-package :cl-user)

(defstruct token
  (literal)
  (symbol))

;; (defparameter *test* "==100,1@(.*(A-Z))$0$10![w:file.txt]$foo[p]$foo$[100+1]")
(defun tokenize (input-stream)
  "takes tokens from the *input-stream* and processes them util the end"
  (let* ((directive (tokenize-directive (with-output-to-string (tmp-stream)
                                          (loop for i from 0 to 1
                                                for c = (read-char input-stream nil) do
                                                  (write-char c tmp-stream)))))
         (tokens `(,directive)))
    (loop :for c = (read-char input-stream nil)
          :while c :do
            (cond ((eq c #\[) (push (tokenize-[ c) tokens))
                  ((eq c #\]) (push (tokenize-] c) tokens))
                  ((eq c #\:) (push (tokenize-colon c) tokens))
                  ((eq c #\\) (push (tokenize-escape input-stream c) tokens))
                  ((eq c #\() (push (tokenize-regex input-stream) tokens))
                  ((eq c #\@) (push (tokenize-reader-end c) tokens))
                  ((eq c #\,) (push (tokenize-comma c) tokens))
                  ((eq c #\$) (push (tokenize-object input-stream c) tokens))
                  ((eq c #\.) (push (tokenize-object input-stream c) tokens))
                  ((eq c #\|) (push (tokenize-bar input-stream c) tokens))
                  ((eq c #\*) (push (tokenize-star input-stream c) tokens))
                  ((eq c #\') (filter-comment input-stream c))
                  ((operatorp input-stream c) (push (tokenize-operator c) tokens))
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
  "takes a object reference out of the stream denoted by foo or 10 10.0 foo.0 .10 20."
  (unread-char c input-stream) ;; unread c to prevent the first char from being cut off 
  (let* ((floating-point nil)
         (hashtag nil)
         (object (with-output-to-string (output-stream)
                   (loop :for c := (read-char input-stream nil)
                         :for i from 0
                         :if (null c) :do
                           (loop-finish)
                         :else :if (and (eq c #\.) (null floating-point)) :do
                           (write-char c output-stream)
                           (setf floating-point t)
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
  "looks for a specific way to parse a file"
  (make-token :literal directive :symbol 'reader))

(defun filter-comment (input-stream c)
  "filters everything between ' and '"
  (loop :for c := (read-char input-stream nil)
        :if (null c) :do
            (loop-finish)
        :else :if (eq c #\') :do
          (loop-finish)))

;; (defun tokenize-space (input-stream c)
;;   "tokenizes the space token in ltpl"
;;   ;; TODO INDEX -1 error 
;;   (when (and (eq c #\_) (eq (read-char input-stream) #\)))
;;     (return-from tokenize-space (make-token :literal "__" :symbol 'operator)))
;;   (unread-char input-stream)
;;   (make-token :literal #\_ :symbol 'operator))

(defun operatorp (input-stream c)
  (let ((operators '(#\/ #\+ #\- #\% #\! #\~)))
    (dolist (op operators)
      (when (eq op c)
        (return-from operatorp t)))))

(defun tokenize-bar (input-stream c)
  "determines if its a the split at or the split operator"
  (when (and (char= c #\|) (char= (read-char input-stream) #\|))
    (return-from tokenize-bar (make-token :literal "||" :symbol 'operator)))
  (unread-char input-stream)
  (make-token :literal #\| :symbol 'operator))

(defun tokenize-star (input-stream c)
  "determines if its a exponent or a multiplication"
  ;;  (break (format nil "~a" c))
  (when (and (char= c #\*) (char= (read-char input-stream) #\*))
    (return-from tokenize-star (make-token :literal "**" :symbol 'operator)))
  (unread-char c input-stream)
  (make-token :literal #\* :symbol 'operator))

(defun tokenize-escape (input-stream c)
  "tokenizes an escape sequence \asl;dkjfaksdfja;sdfkj this is valid "
  (let* ((escape-seq (with-output-to-string (output-stream)
                       (loop :for c := (read-char input-stream nil)
                             :for i from 0
                             :if (null c) :do
                               (loop-finish)
                             :else :if (alpha-char-p c) :do
                               (write-char c output-stream)
                             :else :do
                               (unread-char c input-stream)
                               (loop-finish)))))
    (make-token :literal escape-seq :symbol 'esc)))

(defun tokenize-operator (c)
  (make-token :literal (string c) :symbol 'operator))

(defun tokenize-colon (c)
  (make-token :literal (string c) :symbol 'arg-delim))

(defun tokenize-comma (c)
  (make-token :literal (string c) :symbol 'range-delim))

(defun tokenize-[ (c)
  (make-token :literal (string c) :symbol 'action-begin))

(defun tokenize-] (c)
  (make-token :literal (string c) :symbol 'action-end))

(defun tokenize-reader-end (c)
  (make-token :literal (string c) :symbol 'reader-end))
