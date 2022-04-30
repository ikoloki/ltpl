(defpackage :object-stream-tester
  (:use :cl)
  (:local-nicknames (:gen :generic-cl)))

(in-package :object-stream-tester)

(defmacro do-check (name expected &body body)
  `(progn
     (let ((*print-right-margin* 150))
       (if (gen:= ,expected ,@body)
	   (format t "Test: ~a Succeded ~a~%" ,name ,@body)
	   (format *error-output* "In ~a Test:  Failed Expected: ~a But found: ~a ~%" ,name ,expected ,@body)))))

(defun  object-stream-reader-tester ()
  (let ((test-cases '(() (1 2 3) ("this" 'is #\a 'test))))

    (do-check "Read-Test 1" '()
      (read-object (make-object-stream (car test-cases))))

    (do-check "Read-Test 2" 1
      (read-object (make-object-stream (cadr test-cases))))

    (do-check "Read-Test 3" "this"
      (read-object (make-object-stream (caddr test-cases))))

    (do-check "Read-Objects-Test 1" '(nil nil nil)
      (read-objects (make-object-stream (car test-cases)) 3))

    (do-check "Read-Objects-Test 2" '(1 2 3)
      (read-objects (make-object-stream (cadr test-cases)) 3))

    (do-check "Read-Objects-Test 3" '("this" 'is #\a)
      (read-objects (make-object-stream (caddr test-cases)) 3))

    (do-check "Unread-Object" 1
      (let ((x (make-object-stream '(1 2 3))))
	(read-object x)
	(unread-object x)))))

(defun object-stream-writer-tester ()
  (let ((test-cases '(() (1 2 3) ("this" 'that #\a "the other"))))

    (do-check "Write-Object-Test 1" '(1)
      (write-object (make-object-stream (car test-cases)) 1))

    (do-check "Write-Object-Test 2" '(1 2 3 1)
      (write-object (make-object-stream (cadr test-cases)) 1))

    (do-check "Write-Object-Test 3" '("this" 'that #\a "the other" 1)
      (write-object (make-object-stream (caddr test-cases)) 1))

    (do-check "Write-Objects-Test 1" '(1 2 3)
      (write-objects (make-object-stream '(1 2 3)) '(1 2 3)))

    (do-check "Write-Objects-Test 2" '(1 2 3)
      (write-objects (make-object-stream '()) '(1 2 3)))

    (do-check "Write-Objects-Test 3" '(a b c)
      (write-objects (make-object-stream '()) '(a b c)))

    (do-check "Write-Objects-Test 1" '(1 2 3)
      (let ((x (make-object-stream '())))
        (write-objects x '(1 2 3))
        (unwrite-objects x 3)))

    (do-check "Unwrite-Object-Test 1" 1
      (let ((x (make-object-stream '())))
        (write-object x 1)
        (unwrite-object x)))

    (do-check "Unwrite-Object" '(1 2 3)
      (let ((x (make-object-stream '())))
	(write-objects x '(1 2 3))
        (unwrite-objects x)
        (slot-value x 'objects)))))

(defun object-stream-peek-test ()
  (let ((test-cases '(() (1 2 3) (this that #\a "the other"))))

    (do-check "Peek Object Test 1" '()
      (peek-object (make-object-stream (car test-cases))))

    (do-check "Peek Object Test 2" 1
      (peek-object (make-object-stream (cadr test-cases))))

    (do-check "Peek Object Test 3" 'this
      (peek-object (make-object-stream (caddr test-cases))))))

(defun convert-object-stream-to-tester ()

  (do-check "Convert Object to List 1" '(1 2 3 4)
    (convert-object-stream-to 'list (make-object-stream '(1 2 3 4))))

  (do-check "Convert Object to List 2" '(a b c)
    (convert-object-stream-to 'list (make-object-stream '(a b c))))

  (do-check "Convert Object to List 3" '("hello" "world")
    (convert-object-stream-to 'list (make-object-stream '("hello" "world"))))

  (do-check "Convert Object to Array 1" #0A(1 2 3 4)
    (convert-object-stream-to 'vector (make-object-stream '(1 2 3 4))))

  (do-check "Convert Object" #0A(a b c)
    (convert-object-stream-to 'vector (make-object-stream '(a b c))))

  (do-check "Convert Object" #0A("hello" "world")
    (convert-object-stream-to 'vector (make-object-stream '("hello" "world")))))

(defun run-tests ()
  (object-stream-reader-tester)
  (object-stream-writer-tester)
  (object-stream-peek-test)
  (convert-object-stream-to-tester))

