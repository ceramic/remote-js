(defsystem remote-js
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:clack
               :trivial-ws
               :cl-markup
               :find-port)
  :components ((:module "src"
                :serial t
                :components
                ((:file "remote-js"))))
  :description "Send JavaScript from Common Lisp to a browser."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op remote-js-test))))
