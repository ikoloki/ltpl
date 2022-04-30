(in-package :object-stream)


(defclass object-write-stream (object-stream)
  ())

(defun make-object-write-stream (objects)
  (make-instance 'object-write-stream :objects objects))
   
(defmethod read-object (object-read-stream)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM 
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod read-objects (object-read-stream objects)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod unread-object (object-read-stream)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod unread-objects (object-read-stream number)
  (error "UNABLE TO READ TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))
