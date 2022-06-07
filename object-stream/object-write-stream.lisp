(in-package :object-stream)

(defclass object-write-stream (object-stream)
  ())

(defun make-object-write-stream (objects)
  (make-instance 'object-write-stream :objects objects))

(defmethod read-object ((object-stream object-write-stream))
  (error "UNABLE TO READ FROM OBJECT-WRITE-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod read-objects ((object-stream object-write-stream) number)
  (error "UNABLE TO READ FROM OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod unread-object ((object-stream object-write-stream))
  (error "UNABLE TO READ FROM OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))

(defmethod unread-objects ((object-stream object-write-stream) number)
  (error "UNABLE FROM READ FROM OBJECT-READ-STREAM
OBJECTS MUST BE OF TYPE OBJECT-STREAM OR READ-STREAM" #\RETURN))
