(in-package :object-stream)

(defclass object-bidirectional-stream (object-read-stream object-write-stream)
  ())

(defun make-object-bidirectional-stream (object-bidirectional-stream object-data)
  (make-instance 'object-bidirectional-stream :objects object-data))
