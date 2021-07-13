;; display.lisp

;; this file contains gloabal variables that correspond to X11 resources
;; and functions that acess them

;; screen, roots and display are defined as funcitons, to allow less painfull
;; transition to multi-screen implementaiton

;; https://sharplispers.github.io/clx/Displays.html#Displays
;; A particular X server, together with its screens and input devices, is called a display. The CLX
;; display object contains all the information about the particular display and its screens, as well
;; as the state that is needed to communicate with the display over a particular connection.

(in-package :lwm)

(defparameter *display* nil "Display.")
(defparameter *screens* nil "Screens.")

(defun screen ()
  "Get default screen."
  (car *screens*))

(defun roots ()
  "Retrun the list of root windows"
  (mapcar (lambda (s) (xlib:screen-root s))
	  *screens*))

(defun root (&optional (n 0))
  "Return the current root window or Nth root window."
  (xlib:screen-root (elt *screens* n)))

(defun display ()
  "Get display."
  *display*)

(defun substructure-redirect (&optional window)
  (setf (xlib:window-event-mask window) '(:substructure-notify :substructure-redirect)))

(defun map-existing-on-start ()
  (dolist (w (xlib:query-tree (root)))
    (when (eql (xlib:window-map-state w) :viewable)
      (add-to-managed w))))

(defun x-connect (&key display-name)
  "Connect to X server and request display, screen and root window.
If :DISPLAY-NAME is provided, connect to it.
If not, connect to the default display."
  (if display-name
      (progn (setf *display* (xlib:open-default-display display-name))
	     (set-env "DISPLAY" display-name))
      (setf *display* (xlib:open-default-display)))
  (setf *screens* (xlib:display-roots *display*))
  (substructure-redirect (root))
  (map-existing-on-start))

(defun x-disconnect ()
  "Disconnect from X server."
  (xlib:close-display *display*))
