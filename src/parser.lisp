(in-package :ltpl)

;; <reader-directive> ::= <reader>

;; <action> ::= <action-begin object-primitive arg-seperator object> | <action-begin action-end>

;; <offset-subfield> ::= <ref begin-action action-end> | <ref begin-action escape-sequence action-end>

;; <range> ::= <object range-seperator object>

;; <expression> ::= <binary-expression operator object> | expression 

;; <binary-expression> ::= <object operator object>

;; <object> ::= <ref object-primitive> | <object-primitive> | <escape-sequence> | <unary object-primitive>

;; <escape-sequence> ::= <esc object-primitive>

;; <inverse-object> ::= <unary object-primitive>

;; <unary> ::= "-"



(defun parse (input-stream)    
  (let ((stack '()))
	(loop :with next = (read-object input-stream)
	      :with symbol = (token-symbol next)
	      :while next :do ;; if read-object reads nil then it exits
			      (cond ((eq symbol 'reader) (parse-reader next stack))
				    ((eq symbol 'object-primitive)) (parse-object input-stream)
				    ((eq symbol 'esc) (parse-esc input-stream stack))
				    ((eq symbol 'reader) (parse-reader input-stream stack))
				    ((eq symbol 'operator) (parse-operator input-stream stack))
				    ((eq symbol 'action-begin) (parse-action-begin input-stream stack))
				    ((eq symbol 'action-end) (parse-action-end input-stream stack))
				    ((eq symbol 'range-seperator) (parse-range-seperator input-stream stack))
				    (t (write-object *parser-object-stream* (cons next -2)))))  ;; -2 unresolved token : next
  stack))

(defun parse-ref (input-stream stack)
  (let ((token (read-object input-stream)))    
    (cond ((eq token 'action-offset-begin) (parse-offset-begin input-stream stack))
	  ((eq token 'object-primitive)
	   (push stack (make-token :literal (concatenate 'string "$" (token-literal (parse-object input-stream))) :symbol 'object-ref))))
	  (t (write-object *parser-error-stream* (cons token -1))))) ;; -1 invalid name conserning the object : tokenp

(defun parse-inner-offset-expression (input-stream stack)
  (concatenate 'string "$[" (loop :with lst = '()
				  :for token = (read-object input-stream)
				  :until (eq (token-symbol token) 'action-end)
				  :if (eq (token-symbol token) 'object-primitive) do 
				    (push (parse-object input-stream) lst)
				  :if (eq (token-symbol token) 'ref) do
				    (push (parse-ref input-stream stack) lst)
				  :if (eq (token-symbol token) 'operator) do
				    (push (parse-operator input-stream stack) lst)
				  :else do
				    (write-object *parser-error-stream* (cons token -3)) ;; invalid token of: within index-offset
				  :finally (coerce lst 'string))))

(defun parse-offset-begin (input-stream stack)
  (let ((token (read-object input-stream)))
    (unless (eq token 'action-offset-end)
      (push stack (make-token :literal (parse-inner-offset-expression input-stream stack) :symbol 'index-offset)))
    (make-token :literal "$[]" :symbol 'index-offset)))


(defun parse-object (input-stream)
  (read-object input-stream))

(defun parse-reader (token stack)
  (let ((mode '()))
    (cond ((eq (token-literal token) "==")
	   (setf mode 'left-right))
	  ((eq (token-literal token) "--")
	   (setf mode 'one-line))
	  ((eq (token-literal token) "||")
	   (setf mode 'column))
	  (t (write-object *parser-error-stream* (cons token -4))))
    (push (make-token :literal (token-literal token) :symbol mode) stack)))


(defun parse-action-begin (input-stream stack)
  (let ((token (read-object input-stream)))
    (push (cond ((eq token 'object-primitive) (parse-object input-stream))
		((eq token 'ref) (parse-ref input-stream stack))
		((eq token 'esc) (parse-esc input-stream stack))
		((eq token 'range-seperator) (parse-range-seperator input-stream stack))
		((eq token 'arg-seperator) (parse-arg-seperator))
		((eq token 'action-end) (parse-action-end input-stream stack)))
	  stack)))
