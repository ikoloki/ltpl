;; (in-package :object-stream)
(in-package :ltpl)

(defclass object-read-stream (object-stream)
  ())

(defun make-object-read-stream (objects)
  (make-instance 'object-read-stream :objects objects))
   
(defmethod write-object (object-read-stream object)
  (error "UNABLE TO WRITE TO OBJECT-READ-STREAM 
 OBJECTS MUST BE OF TYPE OBJECT-STREAM OR WRITE-STREAM" #\RETURN))

(defmethod unwrite-object (object-read-stream)
  (error "UNABLE TO WRITE TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR WRITE-STREAM" #\RETURN))

(defmethod write-objects (object-read-stream objects)
  (error "UNABLE TO WRITE TO OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR WRITE-STREAM" #\RETURN))
