(in-package :ltpl)

(defun string-split (string delim)
  (let ((index (search delim string)))
    (cons (subseq string 0 index)
          (when index
            (string-split (subseq string (+ index 1)) delim)))))

(defun not-null (object)
  (not (null object)))

(defstruct node
  (data)
  (left)
  (right))


(defun insert-node (n value)
  (cond ((null (node-data n))
         (setf (node-data n) value))

        ((and (null (node-right n))
              (null (node-left n)))
         (setf (node-left n) (make-node :data value :left nil  :right nil)))

        ((and (null (node-right n))
              (not-null (node-left n)))
         (setf (node-right n) (make-node :data value :left nil :right nil)))

        ((and (not-null (node-right n))
              (not-null (node-left n)))
         (insert-node (node-left n) value))))

(defun serialize-tree (node)
  (cond ((and (null (node-left node))
              (null (node-right node)))
         (node-data node))

        ((null (node-left node))
         (append (list (serialize-tree node)
                       (serialize-tree (node-left node)))))

        ((null (node-right node))
         (append (serialize-tree node)
                 (serialize-tree (node-right node))))

        (t (append (list (node-data node))
                   (append (list (serialize-tree (node-left node))
                                 (serialize-tree (node-right node))))))))



(defun find-root (root val comparator)
  (when root
    (if (funcall comparator val (node-data root))
        root
        (or (find-root (node-left root) val comparator)
            (find-root (node-right root) val comparator)))))

(defmacro enqueue (object queue)
  `(setf ,queue (append ,queue (list ,object))))

(defmacro dequeue (queue)
  `(setf ,queue (butlast ,queue)))
