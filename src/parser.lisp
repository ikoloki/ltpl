(in-package :ltpl)

;; <reader-directive> ::= <reader>

;; <action> ::= <action-begin object-primitive arg-seperator object> | <action-begin action-end>

;; <offset-subfield> ::= <ref begin-action action-end> | <ref begin-action escape-sequence action-end>

;; <range> ::= <object range-seperator object>

;; <binary-expression> ::= <object operator object>

;; <object> ::= <ref object> | <object-primitive> | <escape-sequence>

;; <escape-sequence> ::= <esc object-primitive>

;; <inverse-object> ::= <unary object-primitive>

;; <unary> ::= "-"

(defstruct bin-tree
  (data)
  (left)
  (right))

(defun insert-left (tree data)
  (print "hello"))

(defun insert-right (tree data)
  (print "hello"))

 (defun parse-ref (tree)
   ;;(push *ast* (make-token :literal "$" :symbol 'ref))
   (let ((token (next))
	(cond ((eq token 'action-begin)
	       (parse-action))
	      ((eq token 'object-primitive)
	       (parse-object))
	
	 
(defun parse (tokens)
  (let ((token-stack '())
	(ast '(())))

    (cond ((eq (token-symbol tokens) 'ref)
	   (parse-ref))
	  ((eq (token-symbol token) 'object-primitive)
	   (parse-object))
	  ((eq (token-symbol token))
