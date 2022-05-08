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

;; <unary> ::= "-" | "!" | : | |

(defstruct parser-error
  (token)
  (data)
  (code)
  (severity))

(defclass parser-error-logger ()
  ((parser-error-stream :initform (make-object-stream :objects '()))))

(defun make-parser-error-logger ()
  (make-instance 'parser-error-stream))

(defmethod parser-log-error (logger error)
  (write-object error (slot-value logger 'parser-error-stream)))

(defmethod parser-logger-report-error (logger token error code severity)
  (parser-log-error logger (make-parser-error :token token :data error :code code :severity severity)))

(defmethod parser-error-dump-errors (logger)
  (loop for error in (convert-object-stream-to (slot-value logger 'parser-error-stream))
        ))

(defun first-parse (input-stream)
  (let* ((error-handler (make-parser-error-logger))
         (token (peek-object input-stream))
         (ast (make-node :data nil :left nil :right nil))
         (symbol (token-symbol token)))

    (case symbol
      (reader (parse-reader token input-stream error-handler ast))
      (parse-object-ref (parse-object-ref token input-stream error-handler ast))
      (object-primitive (parse-object-primitive token input-stream error-handler ast))
      (action-begin (parse-action-begin token input-stream error-handler ast))
      (action-end (parse-action-end token input-stream ast))
      (operator (parse-operator token input-stream error-handler ast))
      (terminator (parse-terminator token input-stream ast))
      (otherwise (parser-logger-report-error token error-handler)))))

(defun parse-reader (token input-stream error-handler ast)
  (let* ((literal (read-from-string (token-literal token)))
         (next (peek-object input-stream))
         (next-symbol (token-symbol next)))

    (case literal
      (-- (insert-node ast (make-token :literal (string literal) :symbol 'line)))
      (== (insert-node ast (make-token :literal (string literal) :symbol 'normal)))
      (|| (insert-node ast (make-token :literal (string literal) :symbol 'columns)))
      (<= (insert-node ast (make-token :literal (string literal) :symbol 'backwards)))
      (^^ (insert-node ast (make-token :literal (string literal) :symbol 'bottom-up)))
      (otherwise (parser-logger-report-error token error-handler token -1 'error))) ;; parse-reader not found

    (cond ((eq next-symbol 'ref)
           (parse-object-ref next input-stream error-handler ast))
          ((eq next-symbol 'object-primitive)
           (parse-object-primitive next input-stream error-handler ast))
          (t (parser-logger-report-error token error-handler token -2 'error))))) ;; invalid object

(defun parse-object-ref (token input-stream error-handler ast)
  (let ((next (peek-object input-stream))
        (next-symbol (token-symbol)))

    (insert-node ast token)

    (cond ((string= (token-literal next) "$")
           (parse-action input-stream error-handler ast))

          ((eq next-symbol 'object-ref)
           (parse-object-ref token input-stream error-handler ast))

          ((eq next-symbol 'operator)
           (parse-operator input-stream error-handler ast))

          ((eq next-symbol 'arg-seperator)
           (parse-arg token input-stream error-handler ast))

          (t (parser-logger-report-error token error-handler token -3))))) ;; unresolved token

(defun parse-operator (input-stream error-handler ast)
  (let ((stack '())
        (queue '()))
    (loop :for tok := (read-object input-stream)
          :if (eq (token-symbol tok) 'operator) :do
            (push tok stack)
          :else :if (eq (token-symbol tok) 'object) :do
            (enqueue tok queue)
          :else :do
          (loop-finish))

    (loop :for op :in stack :do
      (node-insert ast op)
      (node-insert ast (dequeue queue))
      (node-insert ast (dequeue queue)))))
