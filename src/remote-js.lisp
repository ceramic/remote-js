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
           :+connected-message+
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

(defparameter +connected-message+ "connected")

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
RemoteJS.ws.onopen = function() {
  RemoteJS.send('~A');
};"
            (context-port context)
            +connected-message+)))

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
  (:documentation "Send some JavaScript to evaluate remotely. Return the
  string.")

  (:method ((context context) string)
    (trivial-ws:send (first (trivial-ws:clients (context-server context)))
                     string)
    string))

;;; Buffered context

(defclass buffered-context ()
  ((%context :initarg :context)
   (connected :accessor context-connected-p
              :initform nil
              :documentation "Whether or not the client is connected.")
   (buffer :accessor context-buffer
           :initarg :buffer
           :initform nil
           :type list
           :documentation "A list of JavaScript strings to evaluate."))
  (:documentation "The buffered context stores evaluation commands in a buffer
  until a client connects, then sends all of them at once."))

(defun make-buffered-context (&key (port (find-port:find-port)) (callback +default-callback+))
  "Create a bufferedcontext object."
  (let* ((buf-ctx (make-instance 'buffered-context))
         (ctx (make-instance 'context
                             :port port
                             :callback #'(lambda (message)
                                           (if (string= message +connected-message+)
                                               (setf (context-connected-p buf-ctx) t)
                                               (funcall callback message))))))
    (setf (context-buffer buf-ctx) ctx)
    buf-ctx))

(defmethod eval ((context buffered-context) string)
  "Send JavaScript to evaluate, if the buffer is connected. Otherwise, add the code to the buffer.

If there's anything in the buffer and the client is connected, send it all in
order before evaluating the string."
  (with-slots (%context connected buffer) context
    (if connected
        ;; The client is connected.
        (progn
          ;; Do we have a backlog?
          (when buffer
            ;; Send it all
            (mapcar #'(lambda (message)
                        (eval %context message))
                    ;; In order!
                    (reverse buffer))
            ;; Clean the buffer
            (setf buffer nil))
          ;; Evalutate the string
          (eval %context string))
        ;; The client is not connected, add this to the buffer
        (push string buffer)))
  string)
