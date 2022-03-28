(in-package :object-stream)

(defclass object-read-stream (object-stream)
  ())

(defun make-object-read-stream (objects)
  (make-instance 'object-read-stream :objects objects))
   
(defmethod read-object (object-read-stream object)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM 
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod unread-object (object-read-stream object)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod read-objects (object-read-stream objects)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))
