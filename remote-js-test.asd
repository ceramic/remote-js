(defsystem remote-js-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:remote-js
               :fiveam
               :trivial-open-browser)
  :components ((:module "t"
                :serial t
                :components
                ((:file "remote-js")))))
