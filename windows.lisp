;;; windows.lisp

;; contains functions for window manipulation

(in-package :lwm)

(defun win= (a b)
  "Equality predicate for windows"
  (and (xlib:window-p a) (xlib:window-p b) (xlib:window-equal a b)))

(defun current-window ()
  "Retrun currently focused window."
  (car *windows*))

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

(defun floating-p (window)
  (error "FLOATING-P is not implemented yet."))

(defun place-tiled (window)
  (error "PLACE-TILED is not implemented yet."))

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
(defun place-floating (window x y width height value-mask)
  "Place a floating window according to its size hints."
  (let ((list-mask (loop for i below 4
			 ;; ldb extracts 1 bit at the position i from window value mask
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

;; TODO: implement, probably needs value-mask
(defun place-window (window &optional x y width height value-mask)
  "Set initial geometry of the window."
  ;; if a window is not tiled, honor it's size hints
  (if (floating-p window)
      (place-floating window x y width height value-mask)
      (place-tiled window)))

;; TODO: implement
(defun center (window)
  "Cetner the window respecting its size."
  )

;; TODO: implement
(defun raise (widnow)
  "Raise WINDOW to the top."
  )

;; TODO: implement
(defun focus (window)
  "Focus WINDOW."
  )

;; TODO: implement
(defun next ()
  "Focus next window.")

;; TODO: implement
(defun prev ()
  "Focus previous window.")
