;;;; ull1.asd

(cl:in-package :asdf)

(defsystem :ull1
  :serial t
  :components ((:file "package")
               (:file "ull1")))

(defmethod perform ((o test-op) (c (eql (find-system :ull1))))
  (load-system :ull1)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :ull1-internal :ull1))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

