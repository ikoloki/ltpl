(in-package :object-stream)

(defclass object-read-stream (object-stream)
  ())

(defun make-object-read-stream (object-data)
  (make-instance 'object-read-stream :objects object-data))
			 
(defmethod read-object (object-read-stream)
  (let ((object (nth (slot-value object-read-stream 'index) (slot-value object-read-stream 'objects))))
    (incf (slot-value object-read-stream 'index))
    object))

(defmethod unread-object (object-read-stream)
  (if (= (slot-value object-read-stream 'index) 0)
      nil
      (nth (decf (slot-value object-read-stream 'index)) (slot-value object-read-stream 'objects))))

(defmethod read-objects (object-read-stream number)
  (loop for i from 0 to number do
    (read-object object-read-stream 'objects)))

;; (defmethod write-object (object-read-stream object))
;; (defmethod unwrite-object (object-read-stream object))
;; (defmethod write-objects (object-read-stream objects))
 
