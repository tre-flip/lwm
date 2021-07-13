;;; Make a slynk server (use swank, if you are working with slime)

;; (slynk:create-server :port 4005 :dont-close t)

(in-package :lwm)

(defun set-env (var val)
  "Unportable way of setting an environment variable."
  (sb-posix:setenv var val 1))

(defun cleanup ()
  "Free all X resources."
  (handler-case (ungrab-keybindings)
    (xlib:access-error ()
      (print "Attempt to ungrab keys on disconnected display.")))
  (x-disconnect))

(defun event-loop ()
  "X event loop."
  (do () ((eql (xlib:process-event *display* :handler *handlers* :discard-p t) 'quit))))

(defun main (&key display-name)
  "Connect to X11 server and start the event loop.
If if :DISPLAY-NAME is provided, connect to this display."
  (x-connect :display-name display-name)
  (grab-keybindings)
  (unwind-protect
       (event-loop)
    (cleanup)))

;; remove it later
(defun main-debug ()
  "Run on display :1, which is started by ./run-server.sh"  
  (main :display-name ":1"))
