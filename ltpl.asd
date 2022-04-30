(defsystem "ltpl"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("generic-cl")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "ltpl/tests"))))

(defsystem "ltpl/tests"
  :author ""
  :license ""
  :depends-on ("ltpl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ltpl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
