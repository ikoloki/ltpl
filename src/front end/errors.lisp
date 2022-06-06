(in-package :ltpl)

(defstruct parser-error
  (token)
  (code)
  (severity))

(defparameter *parser-error-stream* (make-object-stream '()))

(defun parser-report-return (parser-error ast)
  (write-object *parser-error-stream* parser-error)
  ast)

(defun parser-report (parser-error input-stream ast)
  (with-token input-stream
    (write-object *parser-error-stream* parser-error)
    (tokenize-unknown-token input-stream ast)))

(defmacro parser-report-warning (token code input-stream ast)
  `(parser-report (make-parser-error :token token :code code :severity 'warning) ,input-stream ,ast))

(defmacro parser-report-error (token code input-stream ast)
  `(parser-report (make-parser-error :token token :code code :severity 'error) ,input-stream ,ast))
