;;; Make a slynk server (use swank, if you are working with slime)

;; (slynk:create-server :port 4005 :dont-close t)

(defpackage #:lwm
  (:use #:common-lisp))

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

(defmacro defhandler (event keys &body body)
  (let ((fn-name (gensym (symbol-name event)))
        (event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
                (declare (ignore ,event-slots))
                ,@body))
       (setf (elt *handlers* (position ,event xlib::*event-key-vector*)) #',fn-name))))


;;; WINDOWS ;;;

(defun win= (a b)
  "Equality predicate for windows"
  (and (xlib:window-p a) (xlib:window-p b) (xlib:window-equal a b)))

(defun managed-p (window)
  "Return window from the managed windows list if it is here."
  (find window *windows* :test #'win=))

(defun window-make-last (window)
  "Make window the CAR of *WINDOWS*.
Used to keep track of focus history."
  (setf *windows* (list* window (remove window *windows* :test #'win=))))

(defun add-to-managed (window)
  "Add WINDOW to the list of managed windows and return WINDOW."
  (unless (or (eql (xlib:window-override-redirect window) :on)
              (transient-for-managed-p window))
    (window-make-last window))
  window)

(defun current-window ()
  "Retrun currently focused window."
  (car *windows*))

(defun remove-from-managed (window)
  "House keeping when window is removed from managed windows. Returns
the window to be focused."
  (car (setf *windows* (remove window *windows* :test #'win=))))

(defun transient-for-managed-p (window)
  "Check if WINDOW is transient of a managed window."
  (loop for id in (xlib:get-property window :WM_TRANSIENT_FOR)
          thereis (find id *windows* :key #'xlib:window-id)))

(defun get-transients-of (window)
  "Get all transient windows of WINDOW."
  (restart-case
      (loop for w in (xlib:query-tree *root*)
            nconc (loop for id in (xlib:get-property w :WM_TRANSIENT_FOR)
                        when (= id (xlib:window-id window))
                          collect w))
    (window-error (c) (declare (ignore c)) nil)
    (match-error (c)
      (format t "~&get-transients-of: ~a ~a~%" c window)
      nil)))

(defun xclass (window)
  "Get X window class."
  (multiple-value-bind (name class)
      (restart-case (xlib:get-wm-class window)
	(window-error (c) (declare (ignore c)) (values "" "I'm probably dead")))
    (declare (ignore name))
    class))


;; TODO: implement
(defun move (window &key (respect-hints t) x y width height dx dy dw dh)
  "Move a window. Returns effective size values."
  )

;; TODO: implement
(defun initial-place (window)
  "Compute initial size an coordinates of the window.")

;; TODO: implement
(defun center (window)
  ""
  )

;; TODO: implement
(defun raise-window (widnow)
  "Raise WINDOW to the top."
  )

;; TODO: implement
(defun focus-window (window)
  "Focus WINDOW."
  )

;; TODO: implement
(defun next-window ()
  "Focus next window.")

;; TODO: implement
(defun pref-window ()
  "Focus previous window.")

;;; KEYBINDINGS ;;;

;; TODO: implementation, docstring
(defun grab-keybindings ()
  "Grab keys that are defined in ..."
  )

;; TODO: implementation, docstring
(defun ungrab-keybindings ()
  "Ungrab keys that are defined in ..."
  )


;;; EVENT HANDLERS ;;;
(defhandler :key-press (window state code)
  )

;; In the two following handlers, the top `window' binding is required
;; else we're using a fresh instance of window without its plist set
;; for instance
(defhandler :button-press (state code child x y)
  (let ((window (managed-p child)))
    (when (and window (eql (xlib:window-override-redirect window) :off))
      )))

(defhandler :motion-notify (event-window root-x root-y time)
  (let ((window (managed-p event-window)))
    ;; with-state https://sharplispers.github.io/clx/Window-Attributes.html#index-with_002dstate
    ;; used to batch drawing requests
    (xlib:with-state (window)
      )))

(defhandler :button-release () (ungrab-mouse))

;; invoked when a window IS displayed
(defhandler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (focus (managed-p window))))

;; invoked when a window is destroyed
(defhandler :destroy-notify (window)
  (when (managed-p window)
    (focus (minus window))))

;; this handler is invoked when a new window is created
;; 1. xlib:map-window displays the window
(defhandler :map-request (window)
  (restart-case (xlib:map-window (add-to-managed window))
    (window-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)
    (value-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)))

;; https://sharplispers.github.io/clx/Structure-Control-Events.html#Structure-Control-Events
;; invoked when a client program sets the x, y, width, height, border-width
;; or stacking priority attributes of a window that has the override-redirect attribute :off. 
;; it's used to allow programs to set their window geometry.
(defhandler :configure-request (window x y width height value-mask)
  (let ((list-mask (loop for i below 4
                         when (= (ldb (byte 1 i) value-mask) 1)
                           nconc (case i
                                   (0 (list :x x))
                                   (1 (list :y y))
                                   (2 (list :width width))
                                   (3 (list :height height))))))
    (xlib:with-state (window)
      (restart-case (when list-mask (apply #'move window list-mask))
        (window-error (c)
          (format t "~&configure-request: ~a ~a~%" c window)
          'processed)
        (value-error (c)
          (format t "~&configure-request: ~a ~a~%" c window)
          'processed)))))

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
