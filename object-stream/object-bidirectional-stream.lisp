(defpackage :object-stream
   (:use :cl)	
   (:local-nicknames))

;; (in-package :object-stream)

(in-package :object-stream-tester)

(defclass object-stream ()
  ((objects
    :initarg :objects)
   (read-objects
    :initform '())))

(in-package :ltpl)
(defgeneric read-object (object-stream))

(defgeneric unread-object (object-stream))
(defgeneric read-objects (object-stream number))
(defgeneric unread-objects (object-stream number))

(defgeneric write-object (object-stream object))
(defgeneric write-objects (object-stream number))
(defgeneric unwrite-object (object-stream))
(defgeneric unwrite-objects (object-stream number))

(defun make-object-stream (object-data)
  (make-instance 'object-stream :objects object-data))

(defun convert-object-stream-to (linear-sequence object-stream)
  (case linear-sequence
    (list (slot-value object-stream 'objects))
    (vector (make-array '() :initial-contents (slot-value object-stream 'objects)))))

(defmethod peek-object (object-stream)
  (let ((obj (read-object object-stream)))
    (unread-object object-stream) obj))

(defmethod write-object (object-stream object)
  (setf (slot-value object-stream 'objects) (append (slot-value object-stream 'objects) (list object))))

(defmethod write-objects (object-stream objects)
  (loop for i in (reverse objects) do
    (write-object object-stream i))
  objects)

(defmethod unwrite-object (object-stream)
  (let ((obj (car (last (slot-value object-stream 'objects)))))
    (setf (slot-value object-stream 'objects) (butlast (slot-value object-stream 'objects))) obj))

(defmethod unwrite-objects (object-stream number)
  (loop for i from 1 to number do
    (unwrite-object object-stream)))

(defmethod read-object (object-stream)
  (let ((object (pop (slot-value object-stream 'objects))))
    (unless (null object)
      (push object (slot-value object-stream 'read-objects)))
    object))

(defmethod read-objects (object-stream number)
  (let ((acc '()))
    (loop for i from 1 to number do
      (setf acc (append acc (list (read-object object-stream)))))
    acc))

(defmethod unread-object (object-stream)
  (unless (null (slot-value object-stream 'read-objects))
    (car (setf (slot-value object-stream 'objects) (append (list (pop (slot-value object-stream 'read-objects)))
						                                               (slot-value object-stream 'objects))))))

(defmethod unread-objects (object-stream number)
  (let ((acc '()))
    (loop :for i from 0 to number do
      (setf acc (append acc (list (read-object object-stream))))
      (unread-object object-stream))
    acc))
