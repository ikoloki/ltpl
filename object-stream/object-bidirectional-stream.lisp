(defpackage :object-stream
  (:use :cl)
  (:local-nicknames))

(in-package :object-stream)

(defclass object-stream ()
  ((objects
    :initarg :objects)
   (read-objects
    :initform '())))

(defgeneric read-object (object-stream))

(defgeneric unread-object (object-stream))
(defgeneric read-objects (object-stream number))

(defgeneric write-object (object-stream object))
(defgeneric unwrite-object (object-stream object))
(defgeneric write-objects (object-stream objects))

(defun make-object-stream (object-data)
  (make-instance 'object-stream :objects object-data))

(defmethod write-object (object-stream object)
  (setf (slot-value object-stream 'objects) (append (slot-value object-stream 'objects) (list object))))

(defmethod unwrite-object (object-stream)
  (setf (slot-value object-stream 'objects) (butlast (slot-value object-stream 'objects))))

(defmethod write-objects (object-stream objects)
  (loop for i in (reverse objects) do
    (write-object object-stream i)))

(defmethod read-object (object-stream)
  (let ((object (pop (slot-value object-stream 'objects))))
    (unless (null object)
      (push object (slot-value object-stream 'read-objects)))
    object))

(defmethod unread-object (object-stream)
  (unless (null (slot-value object-stream 'read-objects))
    (pop (slot-value object-stream 'read-objects))))

(defmethod read-objects (object-stream number)
  (let ((acc '()))
    (loop for i from 0 to number do
      (setf acc (append acc (list (read-object object-stream)))))
    acc))
