(in-package :ltpl)

;; <reader-directive> ::= <reader>

;; <action> ::= <action-begin object-primitive arg-seperator object> | <action-begin action-end>

;; <offset-subfield> ::= <ref begin-action action-end> | <ref begin-action escape-sequence action-end>

;; <range> ::= <object range-seperator object>

;; <expression> ::= <binary-expression operator object> | expression 

;; <binary-expression> ::= <object operator object>

;; <object> ::= <object-ref> |  <object-primitive> | <escape-sequence> | <regex>

;; <escape-sequence> ::= <SEC object-primitive>

;; <inverse-object> ::= <unary object-primitive>

;; <unary> ::= "-" | "!"





;; TODO: Make a fall through function that does something after the error is reported.
;; It would probably just need to get the next token then go from there but im lazy rn :)

(defstruct parser-error
  (token)
  (code)
  (severity))

(defstruct box
  (data))

(defun report-error-return (parser-error)
  (write-object *parser-error-stream* parser-error))

(defparameter *parser-error-stream* (make-object-stream '()))

(defmacro with-next-token (input-stream &body forms)
  `(let* ((next (read-object ,input-stream))
          (next-symbol (token-symbol next)))
     ,@forms))

(defun parse (input-stream)
  "Initial state of the parser. It creates the AST along reading the first token"

  (let* ((token (peek-object input-stream))
         (ast (make-box :data (make-node :data nil :left nil :right nil)))
         (symbol (token-symbol token)))

    (when (null token)
      (report-error-return (make-parser-error :token nil :code -1 :severity 'error))) ;; No input given this literally should never happen but error checking is always good :)

    (parse-reader (token input-stream ast))


(defun parse-reader (token input-stream ast)
  (with-next-token input-stream
    (cond ((string= (token-literal token) "--") (insert-node (box-data ast) (make-token :literal (token-literal token) :symbol 'line)))
          ((string= (token-literal token) "==") (insert-node (box-data ast) (make-token :literal (token-literal token) :symbol 'normal)))
          ((string= (token-literal token) "||") (insert-node (box-data ast) (make-token :literal (token-literal token) :symbol 'columns)))
          ((string= (token-literal token) "<=") (insert-node (box-data ast) (make-token :literal (token-literal token) :symbol 'backwards)))
          ((string= (token-literal token) "^^") (insert-node (box-data ast) (make-token :literal (token-literal token) :symbol 'bottom-up)))
          (t (write-object (make-parser-error :token next :code -3 :severity 'error) *parser-error-stream*))) ;; invalid-parser-reader-found

    (when (null next)
      (report-error-return (make-parser-error :token token :code -4 :severity 'error))) ;;  found end of line after reader-directive

    (case next-symbol
      (object-ref (parse-object-ref next input-stream ast))
      (object-primitive (parse-object-ref next input-stream ast))
      (reader-end (parser-reader-end next input-stream ast))
      (otherwise (write-object (make-parser-error :token next :code -5 :severity 'error) *parser-error-stream*))))) ;; expected object expression or @ (ill figure out a fancy name for it later)

(defun parse-object-ref (token input-stream ast)
  (with-next-token input-stream
    (insert-node (box-data ast) token)

    (cond ((string= (token-literal next) "$") (parse-action token input-stream ast))
          ((null next) (report-error-return (make-parser-error :token token :code -7 :severity 'error)))) ;;found object that is not being enacted on by any action

    (case next-symbol
      (object-ref (parse-object-ref token input-stream ast))
      (range-delim (parse-range token input-stream ast))
      (arg-delim (parse-arg token input-stream ast))
      (otherwise (write-object (make-parser-error :token next :code -8 :severity 'error))))))) ;; unresolved object after object reference

(defun parse-object-primitive (token input-stream ast)
    (with-next-token input-stream
      (insert-node (box-data ast) token)

      (when (null next)
        (report-error-return (make-parser-error :token next :code -9 :severityy 'error))) ;; found object referance that is not being enacted on any object

      (case next-symbol
        (action begin (parse-action next input-stream ast))
        (range-delim (parse-range token input-stream ast))
        (object-ref (parse-object-ref token input-stream ast))
        (operator (parse-operator token input-stream ast))


        (cond ((eq next-symbol 'action-begin)
               (parse-action next input-stream ast))

              ((eq next-symbol 'range-delim)
               (parse-range token input-stream ast))

            ((eq next-symbol 'arg-delim)
             (parse-arg-delim token input-stream ast))

            ((eq next-symbol 'ref)
             (parse-object-ref token input-stream ast))

            ((eq next-symbol 'operator)
             (parse-operator token input-stream ast))

            ((null next)
             (report-error-return (make-parser-error :token token :code -7 :severity 'error))) ;;expected found object that is not being enacted on by any action
            (t (write-object (make-parser-error :token next :code -8 :severity 'error) *parser-error-stream*))))) ;; unresolved token after object

(defun parse-arg (token input-stream ast)
  (with-next-token input-stream
    (when (null next)
      (write-object (make-parser-error :token token :code -9 :severity 'error) *parser-error-stream*)) ;; expected other arguments in action)

    (case next-symbol
      (object-ref (parse-object-ref token input-stream ast))
      (object-primitive (parse-object-primitive token input-stream ast))
      (t (write-object (make-parser-error :token next :code -10 :severity 'error) *parser-error-stream*)))))

(defun parse-action (token input-stream ast)
  (let ((next (peek-object input-stream)))
    (cond ((eq (token-symbol next) 'object-primitive)
           (parse-object-primitive next input-stream ast))
          ((eq (token-symbol next) 'object-ref)
           (parse-object-ref next input-stream ast))
          (t (write-object (make-parser-error :token next :code -7 :severity 'error) *parser-error-stream*)))))

;;  (defun parse-object-primtitive (token input-stream ast)

(defun parse-operator (token input-stream ast)
  (let ((stack '())
        (queue '()))

    ;; (when (null (parse-integer (token-literal tok) :junk-allowed t))
    ;;   (write-object (make-parser-error :token next :code 1 :severity 'warning))) ;; enacting operator

    (loop :for tok := (read-object input-stream)
          :if (eq (token-symbol tok) 'operator) :do
            (push tok stack)
          :else :if (eq (token-symbol tok) 'object) :do
            (enqueue tok queue)
          :else :do
            (write-object (make-parser-error :token tok :code -5 :severity 'error) *parser-error-stream*) ;; unresolved token found in expression
            (loop-finish))

    (loop :for op :in stack :do
      (node-insert (box-data ast) op)
      (node-insert (box-data ast) (dequeue queue))
      (node-insert (box-data ast) (dequeue queue)))))

(defun action-end (token input-stream ast)
  (let* ((next (peek-object input-stream))
         (next-symbol (token-symbol next)))

    (when (null next)
      (report-error (make-parser-error :token token :code -8 :severity 'error)))


    )))))

;; (defun action-end (parse-action-end token input-stream ast)
;; )

