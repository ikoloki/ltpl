(defpackage :object-stream
  (:use :cl)
  (:local-nicknames))

(in-package :object-stream)

(defclass object-stream ()
  ((objects
   :initarg :objects)
   (index
    :initform 0)))

(defgeneric read-object (object-stream))

(defgeneric unread-object (object-stream))
(defgeneric read-objects (object-stream number))

(defgeneric write-object (object-stream object))
(defgeneric unwrite-object (object-stream))
(defgeneric write-objects (object-stream objects))

(defun make-object-stream (object-data)
  (error "CANNOT MAKE-OBJECT-STREAM. ONLY ITS SUBCLASSES OF:
 WRITE-OBJECT-STREAM,
 READ-OBJECT-STREAM AND
 BIDIRECTIONAL-OBJECT-STREAM"))

(defmethod write-object (object-stream object)
  (error "CANNOT WRITE-OBJECT TO OBJECT-STREAM. 
 ONLY ITS SUBCLASSES OF:
 WRITE-OBJECT-STREAM AND 
 BIDIRECTIONAL-OBJECT-STREAM"))

(defmethod unwrite-object (object-stream)
  (error "CANNOT UNWRITE-OBJECT TO OBJECT-STREAM. ONLY ITS SUBCLASSES OF:
  WRITE-OBJECT-STREAM AND 
  BIDIRECTIONAL-OBJECT-STREAM"))

(defmethod write-objects (object-stream number)
  (error "CANNOT WRITE-OBJECTS TO OBJECT-STREAM. ONLY ITS SUBCLASS OF:
WRITE-OBJECT-STREAM AND BIDIRECTIONAL-OBJECT-STREAM"))

(defmethod read-object (object-stream)
  (error "CANNOT READ-OBJECT FROM OBJECT-STREAM. ONLY ITS SUBCLASS OF:
 READ-OBJECT AND
 BIDRIECTIONAL-OBJECT-STREAM"))

(defmethod unread-object (object-stream)
  (error "CANNOT UNREAD-OBJECT FROM OBJECT-STREAM. 
ONLY ITS SUBCLASS OF READ-OBJECT AND 
BIDRIECTIONAL-OBJECT-STREAM"))

(defmethod read-object (object-stream)
  (error "CANNOT READ-OBJECTS FROM OBJECT-STREAM. ONLY ITS SUBCLASS OF:
READ-OBJECT AND
 BIDRIECTIONAL-OBJECT-STREAM"))
