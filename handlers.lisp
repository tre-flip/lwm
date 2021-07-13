;; handlers.lisp

;; event handlers are defined here
;; please, keep handler bodies concise, write funcitons for input processing in keys.lisp,
;; funcitons for window manipulation in windows.lisp

(in-package :lwm)

(defparameter *handlers* (make-list (length xlib::*event-key-vector*)
                              :initial-element #'(lambda (&rest slots))))

(defmacro defhandler (event keys &body body)
  (let ((fn-name (gensym (symbol-name event)))
        (event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
                (declare (ignore ,event-slots))
                ,@body))
       (setf (elt *handlers* (position ,event xlib::*event-key-vector*)) #',fn-name))))


(defhandler :key-press (window state code)
  (resolve-key state code window))

;; keyboard shortcuts are handled here
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

;; invoked when a window is destroyed
(defhandler :destroy-notify (window)
  (when (managed-p window)
    (focus (remove-from-managed window))))

;; invoked when a window IS displayed
(defhandler :map-notify (window override-redirect-p)
  (unless override-redirect-p
    (focus window)))

;; this handler is invoked right before the window is created
;; 1. xlib:map-window displays the window
(defhandler :map-request (window)
  ;; restart-case protects the WM from hanging when an error occurs
  (restart-case (xlib:map-window (add-to-managed window))
    (window-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)
    (value-error (c)
      (format t "~&map-request: ~a ~a~%" c window)
      'processed)))

;; https://sharplispers.github.io/clx/Structure-Control-Events.html#Structure-Control-Events
;; https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html#events:ConfigureRequest
;; https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html#requests:ConfigureWindow
;; value-mask:
;; x	INT16
;; y	INT16
;; width	CARD16
;; height	CARD16
;; border-width	CARD16
;; sibling	WINDOW
;; stack-mode	{ Above, Below, TopIf, BottomIf, Opposite } 
;; invoked when a client program sets the x, y, width, height, border-width
;; or stacking priority attributes of a window that has the override-redirect attribute :off. 
;; it's used to allow programs to set their window geometry.
(defhandler :configure-request (window x y width height value-mask)
  ;; inspect first 4 bits of value-mask
  (place-window window x y width height value-mask))

;; EWMH messages are handled here
;; https://sharplispers.github.io/clx/Client-Communications-Events.html#index-_003aclient_002dmessage
(defhandler :client-message (window type)
  )

;; EWMH properties are handled here
;; https://sharplispers.github.io/clx/Client-Communications-Events.html#index-_003aclient_002dmessage
(defhandler :property-notify (window atom state)
  )
