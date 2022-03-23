(in-package :ltpl)

(defvar *error-stack* '())

(defstruct error
  (code)
  (severity))
