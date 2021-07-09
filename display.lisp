;; display.lisp

;; this file contains gloabal variables that correspond to X11 resources

;; screen, root and display are defined as funcitons, to allow less painfull
;; transition to multi-screen implementaiton

;; https://sharplispers.github.io/clx/Displays.html#Displays
;; A particular X server, together with its screens and input devices, is called a display. The CLX
;; display object contains all the information about the particular display and its screens, as well
;; as the state that is needed to communicate with the display over a particular connection.

(defparameter *display* nil "Display.")
(defparameter *screens* nil "Screens.")

(defun screen ()
  "Get default screen."
  (car *screens*))

(defun roots ()
  "Retrun the list of root windows"
  (mapcar (lambda (s) (xlib:screen-root s))
	  *screens*))

(defun display ()
  "Get display."
  *display*)

(defun x-connect (&key display-name)
  "Connect to X server and request display, screen and root window."
  (if display-name
      (setf *display* (xlib:open-default-display display-name))
      (setf *display* (xlib:open-default-display)))
  (setf *screens* (xlib:display-roots *display*)))

(defun x-disconnect ()
  "Disconnect from X server."
  (xlib:close-display *display*))
