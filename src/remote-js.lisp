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

(defparameter +default-callback+
  #'(lambda (message)
      (declare (ignore message))
      nil))

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
            :documentation "The server handler.")
   (callback :accessor context-callback
             :initarg :callback
             :initform +default-callback+
             :documentation "The function that is called when the client sends a
 message."))
  (:documentation "A context object."))

(defun make-context (&key (port (find-port:find-port)) (callback +default-callback+))
  "Create a context object."
  (let ((ctx (make-instance 'context
                            :port port
                            :callback callback)))
    (setf (context-server ctx)
          (trivial-ws:make-server
           :on-connect #'(lambda (server)
                           (declare (ignore server)))
           :on-disconnect #'(lambda (server)
                              (declare (ignore server)))
           :on-message #'(lambda (server message)
                           (declare (ignore server))
                           (funcall (context-callback ctx) message))))
    ctx))

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
            "var RemoteJS = {};
RemoteJS.ws = new WebSocket(\"ws://localhost:~D/\");

RemoteJS.send = function(data) {
  RemoteJS.ws.send(data);
};

RemoteJS.ws.onmessage = function(evt) {
  eval(evt.data);
};
RemoteJS.ws.onconnect = function() {
  RemoteJS.send('connected');
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
