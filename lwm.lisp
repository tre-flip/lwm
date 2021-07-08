;;; Make a slynk server (use swank, if you are working with slime)

;; (slynk:create-server :port 4005 :dont-close t)

(in-package :lwm)

;;; SPECIAL VARIABLES ;;;
(defparameter *display* nil "Main display.")
(defparameter *screen* nil "Main screen.")
(defparameter *root* nil "Root window.")
(defparameter *windows* nil
  "List of managed windows.
Focused window is put to the beginning of this list.")
(defparameter *direct-shortcuts* nil "Direct shortcuts alist.")
(defparameter *handlers* (make-list (length xlib::*event-key-vector*)
                              :initial-element #'(lambda (&rest slots))))

;;; WINDOWS ;;;


;;; EVENT HANDLERS ;;;

;; Restart functions
(defmacro defrestart (name)
  (let ((fname (intern (concatenate 'string "RESTART-" (symbol-name name)))))
    (with-gensyms (c restart)
      `(defun ,fname (,c)
         (let ((,restart (find-restart ',name)))
           (when ,restart
             (invoke-restart ,restart ,c)))))))


;;; INIT, MAIN, CLEANUP ;;;

(defun init (&optional display)
  "Request X resources."
  (if display
      (setf *display* (xlib:open-default-display display))
      (setf *display* (xlib:open-default-display)))
  (setf *screen* (xlib:display-default-screen *display*))
  (setf *root* (xlib:screen-root *screen*))
  (grab-keybindings))

(defun cleanup ()
  "Free all X resources."
  (xlib:close-display *display*)
  (ungrab-keybindings))

(defun event-loop ()
  "X event loop."
  (do () ((eql (xlib:process-event *display* :handler *handlers* :discard-p t) 'quit))))

(defun debug-main ()
  "Display :1 must be craeated by Xephyr before calling this funciton."
  (init ":1.0")
  (unwind-protect
       (event-loop)
    (cleanup)))

(defun main (&key debug)
  "Start the event loop."
  (init)
  (unwind-protect
       (event-loop)
    (cleanup)))
