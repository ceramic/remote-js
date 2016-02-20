# remote-js

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

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.
