(in-package :object-stream)


(defclass object-write-stream (object-stream)
  ())

(defun make-object-write-stream (object-data)
  (make-instance 'object-write-stream :objects object-data))

(defmethod write-object (object-write-stream object)
  (push object (slot-value object-write-stream 'objects)))

(defmethod unwrite-object (object-write-stream)
  (pop (slot-value object-write-stream 'objects))
  (decf (slot-value object-write-stream 'index)) nil)

(defmethod write-objects (object-write-stream objects)
  (loop for i in (reverse objects) do
    (write-object object-write-stream i)))
