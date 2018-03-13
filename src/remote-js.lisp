(in-package :cl-user)
(defpackage remote-js
  (:use :cl)
  (:shadow :eval)
  (:export :context
           :context-address
           :context-port
           :context-server
           :context-running-p
           :make-context
           :start
           :stop
           :+connected-message+
           :js
           :html
           :eval
           :buffered-context
           :make-buffered-context
           :context-connected-p)
  (:documentation "The main package."))
(in-package :remote-js)

(defparameter +default-callback+
  #'(lambda (message)
      (declare (ignore message))
      nil))

(defclass context ()
  ((address :reader context-address
            :initarg :address
            :initform trivial-ws:+default-address+
            :type string
            :documentation "The address the WebSockets server will run on.")
   (port :reader context-port
         :initarg :port
         :initform (find-port:find-port)
         :type integer
         :documentation "The port the WebSockets server will run on.")
   (timeout :reader context-timeout
            :initarg :timeout
            :initform trivial-ws:+default-timeout+
            :type integer
            :documentation "The number of seconds after which the WebSockets server will disconnect an inactive client.")
   (server :accessor context-server
           :initarg :server
           :documentation "The trivial-websockets server.")
   (handler :accessor context-handler
            :initarg :handler
            :documentation "The server handler.")
   (runningp :accessor context-running-p
             :initform nil
             :documentation "Whether the server is running.")
   (record :accessor context-record-p
           :initarg :recordp
           :initform nil
           :type boolean
           :documentation "Whether or not to record sent HTML.")
   (callback :accessor context-callback
             :initarg :callback
             :initform +default-callback+
             :documentation "The function that is called when the client sends a
 message."))
  (:documentation "A context object."))

(defun server-for-context (context)
  (trivial-ws:make-server
   :on-connect #'(lambda (server)
                   (declare (ignore server)))
   :on-disconnect #'(lambda (server)
                      (declare (ignore server)))
   :on-message #'(lambda (server message)
                   (declare (ignore server))
                   (funcall (context-callback context) message))))

(defun make-context (&key
                       (address trivial-ws:+default-address+)
                       (port (find-port:find-port))
                       (timeout trivial-ws:+default-timeout+)
                       (callback +default-callback+)
                       recordp)
  "Create a context object."
  (let ((ctx (make-instance 'context
                            :address address
                            :port port
                            :timeout timeout
                            :recordp recordp
                            :callback callback)))
    (setf (context-server ctx) (server-for-context ctx))
    ctx))

(defgeneric start (context)
  (:documentation "Start the WebSockets server.")

  (:method ((context context))
    (with-slots (address port server handler runningp) context
      (setf handler (trivial-ws:start server port :address address)
            runningp t))))

(defgeneric stop (context)
  (:documentation "Stop the WebSockets server.")

  (:method ((context context))
    (with-slots (handler runningp) context
      (trivial-ws:stop handler)
      (setf runningp nil))))

(defparameter +connected-message+ "connected")

(defparameter +script-template+
  "window.RemoteJS = {};
var RemoteJS = window.RemoteJS;

RemoteJS.send = function(data) {
  RemoteJS.ws.send(data);
};

RemoteJS.connect = function() {
  var ws  = new WebSocket(\"ws://~A:~D/\");

  ws.onmessage = function(evt) {
    (1, eval)(evt.data);
  };

  ws.onopen = function() {
    RemoteJS.send('~A');
  };

  ws.onclose = function() {
    setTimeout(RemoteJS.connect, 1);
  };

  RemoteJS.ws = ws;
};

RemoteJS.connect();")

(defgeneric js (context)
  (:documentation "Return the JS for this context.")

  (:method ((context context))
    (format nil +script-template+
            (context-address context)
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
    (when (context-record-p context)
      (format t "JS: ~A~%" string))
    (let ((client (first (trivial-ws:clients (context-server context)))))
      (if client
          (progn
            (trivial-ws:send client string)
            string)
          (error "No client connected")))))

;;; Buffered context

(defclass buffered-context (context)
  ((%callback)
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

(defmethod initialize-instance :after ((context buffered-context) &key)
  ;; FIXME: is this portable?
  (with-slots (callback %callback connected) context
    ;; Copy the user-provided callback
    (setf %callback callback)
    ;; Wrap the callback in a callback that detects the connection message
    (setf callback
          #'(lambda (message)
              (if (string= message +connected-message+)
                        (setf connected t)
                        (funcall %callback message))))))

(defun make-buffered-context (&key
                                (address trivial-ws:+default-address+)
                                (port (find-port:find-port))
                                (callback +default-callback+)
                                (timeout trivial-ws:+default-timeout+)
                                recordp)
  "Create a buffered context object."
  (let ((ctx (make-instance 'buffered-context
                            :address address
                            :port port
                            :timeout timeout
                            :recordp recordp
                            :callback callback)))
    (setf (context-server ctx) (server-for-context ctx))
    ctx))

(defmethod eval ((context buffered-context) string)
  "Send JavaScript to evaluate, if the buffer is connected. Otherwise, add the code to the buffer.

If there's anything in the buffer and the client is connected, send it all in
order before evaluating the string."
  (with-slots (connected buffer) context
    (if connected
        ;; The client is connected.
        (progn
          ;; Do we have a backlog?
          (when buffer
            ;; Send it all
            (mapcar #'(lambda (message)
                        (call-next-method context message))
                    ;; In order!
                    (reverse buffer))
            ;; Clean the buffer
            (setf buffer nil))
          ;; Evalutate the string
          (call-next-method))
        ;; The client is not connected, add this to the buffer
        (push string buffer)))
  string)
