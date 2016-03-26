# remote-js

[![Build Status](https://travis-ci.org/ceramic/remote-js.svg?branch=master)](https://travis-ci.org/ceramic/remote-js)

Send JavaScript from Common Lisp to a browser.

# Overview

# Usage

## Simple Example

First, we create a context object:

```lisp
(defvar ctx (remote-js:make-context))
```

Then we start the WebSockets server:

```lisp
(remote-js:start ctx)
```

Now, remote-js gives us a function that generates the HTML of a simple page that
connects to this context and notifies it when it's connected. We write the HTML
to `~/test.html`:

```lisp
(with-open-file (stream (merge-pathnames #p"test.html" (user-homedir-pathname))
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (write-string (remote-js:html ctx) stream))
```

Open the file in your browser. Now you can do:

```lisp
(remote-js:eval ctx "alert('hello!')")
```

And you will see the alert box pop up in your browser.

## Talking to the server

remote-js defines a function in the generated HTML, `RemoteJS.send`, which takes
a string and sends it to the server. You can specify a callback for receiving
messages like this:

```lisp
(defvar ctx (remote-js:make-context
              :callback #'(lambda (message) (format t "Received: ~A~%" message))))
```

Then, start everything and generate the HTML file again:

```lisp
(remote-js:start ctx)
(with-open-file (stream (merge-pathnames #p"test.html" (user-homedir-pathname))
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (write-string (remote-js:html ctx) stream))
```

And open `test.html` in your browser.

Now you can send messages to the server like this:

```lisp
CL-USER> (remote-js:eval ctx "RemoteJS.send('hi!')")
Received: hi!
```

**Note:** when a client connects to the server, it sends the string
`remote-js:+connected-message+`.

# Tests

The tests use [trivial-open-browser][tob], and running them will open your
default browser to a temporary file.

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.

[tob]: http://quickdocs.org/trivial-open-browser/
