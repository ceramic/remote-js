(in-package :cl-user)
(defpackage remote-js
  (:use :cl)
  (:shadow :eval)
  (:export :context
           :context-port
           :context-server
           :make-context
           :start
           :stop
           :js
           :html
           :eval)
  (:documentation "The main package."))
(in-package :remote-js)

(defclass context ()
  ((port :reader context-port
         :initarg :port
         :initform (find-port:find-port)
         :type integer
         :documentation "The port the WebSockets server will run on.")
   (server :accessor context-server
           :initarg :server
           :documentation "The trivial-websockets server.")
   (handler :accessor context-handler
            :initarg :handler
            :documentation "The server handler."))
  (:documentation "A context object."))

(defun make-context (&key (port (find-port:find-port)))
  "Create a context object."
  (make-instance 'context
                 :port port
                 :server  (trivial-ws:make-server
                           :on-connect #'(lambda (server)
                                           (declare (ignore server)))
                           :on-disconnect #'(lambda (server)
                                              (declare (ignore server)))
                           :on-message #'(lambda (server message)
                                           (declare (ignore server message))))))

(defgeneric start (context)
  (:documentation "Start the WebSockets server.")

  (:method ((context context))
    (with-slots (port server handler) context
      (setf handler (trivial-ws:start server port)))))

(defgeneric stop (context)
  (:documentation "Stop the WebSockets server.")

  (:method ((context context))
    (with-slots (handler) context
      (trivial-ws:stop handler))))

(defgeneric js (context)
  (:documentation "Return the JS for this context.")

  (:method ((context context))
    (format nil
            "var ws = new WebSocket(\"ws://localhost:~D/\");
ws.onmessage = function(evt) {
  eval(evt.data);
};"
            (context-port context))))

(defgeneric html (context)
  (:documentation "Return the HTML for this context.")

  (:method ((context context))
    (markup:html5
     (:head
      (:meta :charset "utf-8")
      (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1"))
     (:body
      (:script (cl-markup:raw (js context)))))))

(defgeneric eval (context string)
  (:documentation "Send some JavaScript to evaluate remotely.")

  (:method ((context context) string)
    (trivial-ws:send (first (trivial-ws:clients (context-server context)))
                     string)))
