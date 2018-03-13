(in-package :cl-user)
(defpackage remote-js-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :remote-js-test)

(def-suite tests
  :description "remote-js tests.")
(in-suite tests)

(test context
  (let ((received nil)
        (ctx)
        (file (asdf:system-relative-pathname :remote-js #p"t/test.html")))
    (finishes
      (setf ctx (remote-js:make-context
                 :recordp t
                 :callback #'(lambda (message)
                               (setf received message)))))
    (finishes
     (remote-js:start ctx))
    (finishes
     (with-open-file (stream file
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (write-string (remote-js:html ctx) stream)))
    (finishes
     (bt:make-thread
      #'(lambda ()
          (uiop:run-program (format nil "chromium-browser 'file://~A'" (namestring file))))))
    (sleep 1)
    (is
     (stringp (remote-js:eval ctx "RemoteJS.send('test')")))
    (sleep 0.1)
    (is-true
     (string= received "test"))
    (finishes (remote-js:eval ctx "var test='global'"))
    (sleep 0.1)
    (finishes (remote-js:eval ctx "RemoteJS.send(test)"))
    (sleep 0.1)
    (is-true
     (string= received "global"))
    (finishes
     (delete-file file))
    (finishes
     (remote-js:stop ctx))))

(defun run-tests ()
  (run! 'tests))
