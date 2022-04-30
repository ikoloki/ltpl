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
              (not-nulnl (node-left n)))
         (tree-insert (node-left n) value))))
