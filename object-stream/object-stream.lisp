(defpackage :object-stream
  (:use :cl)
  (:local-nicknames))

(defclass object-stream ()
  ((objects
   :initarg :objects)
   (index
    :initform 0)))

;; (defgeneric read-object (object-stream &key end-at-nil end-at-nil-p)
(defgeneric read-object (object-stream))

(defgeneric unread-object (object-stream))
(defgeneric read-objects (object-stream number))

(defgeneric write-object (object-stream object))
(defgeneric unwrite-object (object-stream))
(defgeneric write-objects (object-stream objects))

;; (defgeneric object-stream-p (object-stream))

(defun make-object-stream (object-data)
  (make-instance 'object-stream :objects object-data))

(defmethod write-object (object-stream object)
  (push object (slot-value object-stream 'objects)))

(defmethod unwrite-object (object-stream)
  (pop (slot-value object-stream 'objects)))

(defmethod write-objects (object-stream objects)
  (loop for i in objects do
    (write-object object-stream)))

(defmethod read-object (object-stream)
  (let ((object (nth (slot-value object-stream 'index) (slot-value object-stream 'objects))))
    (incf (slot-value object-stream 'index))
    object))

(defmethod unread-object (object-stream)
  (if (= (slot-value object-stream 'index) 0)
      nil
      (nth (decf (slot-value object-stream 'index)) (slot-value object-stream 'objects))))

(defmethod read-objects (object-stream number)
  (loop for i from 0 to number do
    (read-object object-stream 'objects)))
