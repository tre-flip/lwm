;;; Make a slynk server (use swank, if you are working with slime)

;; (slynk:create-server :port 4005 :dont-close t)

(in-package :lwm)

;;; SPECIAL VARIABLES ;;;


;; Restart functions
(defmacro defrestart (name)
  (let ((fname (intern (concatenate 'string "RESTART-" (symbol-name name)))))
    (with-gensyms (c restart)
      `(defun ,fname (,c)
         (let ((,restart (find-restart ',name)))
           (when ,restart
             (invoke-restart ,restart ,c)))))))

;;; INIT, MAIN, CLEANUP ;;;

(defun cleanup ()
  "Free all X resources."
  (ungrab-keybindings)
  (x-disconnect))

(defun event-loop ()
  "X event loop."
  (do () ((eql (xlib:process-event *display* :handler *handlers* :discard-p t) 'quit))))

(defun debug-main ()
  "Display :1 must be craeated by Xephyr before calling this funciton."
  (init ":1.0")
  (unwind-protect
       (event-loop)
    (cleanup)))

(defun main (&key display-name)
  "Connect to X11 server and start the event loop.
If if :DISPLAY-NAME is provided, connect to this display."
  (x-connect :display-name display-name)
  (grab-keybindings)
  (unwind-protect
       (event-loop)
    (cleanup)))
