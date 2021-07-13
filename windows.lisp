;;; windows.lisp

;; contains functions for window manipulation

(in-package :lwm)

(defparameter *windows* nil
  "List of managed windows.
Focused window is put to the beginning of this list.")

(defparameter *border-width* 3)
(defparameter *border-color* 500)

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
      (loop for w in (xlib:query-tree (root))
            nconc (loop for id in (xlib:get-property w :WM_TRANSIENT_FOR)
                        when (= id (xlib:window-id window))
                          collect w))
    (window-error (c) (declare (ignore c)) nil)
    (match-error (c)
      (format t "~&get-transients-of: ~a ~a~%" c window)
      nil)))

(defun x11-capitalize (string)
  "Returns a capitalize string according to X11 class name
convention."
  (if (char= (elt string 0) #\x)
      (concatenate 'string "X" (string-capitalize (subseq string 1)))
      (string-capitalize string)))

(defun xclass (window)
  "Get X window class."
  (multiple-value-bind (name class)
      (restart-case (xlib:get-wm-class window)
	(window-error (c) (declare (ignore c)) (values "" "I'm probably dead")))
    (declare (ignore name))
    class))

(defun pinned-p (window) (getf (xlib:window-plist window) :pinned))
(defun pin (window) (setf (getf (xlib:window-plist window) :pinned) t))
(defun unpin (window) (remf (xlib:window-plist window) :pinned))
(defun toggle-pin ()
  (if (pinned-p (current-window)) (unpin (current-window)) (pin (current-window))))

(defun correct-size (window x y width height dx dy dw dh)
  "Correct a window's dimensions with its sizehints."
  (let ((hints (xlib:wm-normal-hints window)))
    (when hints (let* ((min-w (or (xlib:wm-size-hints-min-width hints) 1))
                       (min-h (or (xlib:wm-size-hints-min-height hints) 1))
                       (inc-w (or (xlib:wm-size-hints-width-inc hints) 1))
                       (inc-h (or (xlib:wm-size-hints-height-inc hints) 1))
                       (base-w (or (xlib:wm-size-hints-base-width hints) 0))
                       (base-h (or (xlib:wm-size-hints-base-height hints) 0)))
                  (when x (setf x (* inc-w (truncate x inc-w))))
                  (when y (setf y (* inc-h (truncate y inc-h))))
                  (when width
                    (decf width base-w)
                    (setf width (max min-w (+ (* inc-w (truncate width inc-w)) base-w))))
                  (when height
                    (decf height base-h)
                    (setf height (max min-h (+ (* inc-h (truncate height inc-h)) base-h))))
                  (when dx (setf dx (* inc-w (truncate dx inc-w))))
                  (when dy (setf dy (* inc-h (truncate dy inc-h))))
                  (when dh (setf dh (* inc-h (truncate dh inc-h))))
                  (when dw (setf dw (* inc-w (truncate dw inc-w))))))
    (values x y width height dx dy dw dh)))

(defun move (window &key x y width height dx dy dw dh)
  "Move a window with respect to sizehints. Returns effective size
values."
  (multiple-value-bind (x y width height dx dy dw dh)
      (correct-size window x y width height dx dy dw dh)
    ;; if not provided get current geometry
    (unless x (setf x (xlib:drawable-x window)))
    (unless y (setf y (xlib:drawable-y window)))
    (unless width (setf width (xlib:drawable-width window)))
    (unless height (setf height (xlib:drawable-height window)))

    ;; dx, dy, dw and dh are rewritten in absolute form
    (when dx (incf x dx))
    (when dy (incf y dy))
    (when dw (let ((new-w (+ width dw)))
               (cond ((minusp new-w)
                      (incf x new-w)
                      (setf width (abs new-w)))
                     (t (setf width new-w)))))
    (when dh (let ((new-h (+ height dh)))
               (cond ((minusp new-h)
                      (incf y new-h)
                      (setf height (abs new-h)))
                     (t (setf height new-h)))))
    (unless (pinned-p window)
      (setf (xlib:drawable-x window) x
            (xlib:drawable-y window) y
            (xlib:drawable-width window) width
            (xlib:drawable-height window) height))
    (values x y width height dx dy dw dh)))

;; TODO: implement it
(defun floating-p (window)
  (format t "WARNING: floating-p is a stub!")
  t)

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

(defun enable-border (window)
  (setf (xlib:drawable-border-width window) *border-width*
	(xlib:window-border window) *border-color*))

(defun place-window (window &optional x y width height value-mask)
  "Set initial geometry of the window."
  ;; if honor size hints of floating windows
  (enable-border window)
  (if (floating-p window)
      (place-floating window x y width height value-mask)
      (place-tiled window)))

;; Following functions take a list of windows and perfrom an operation on them.
;; They must be composable with funcitons that return a list of windows.

(defun next (windows)
  "Get text matching window"
  )

(defun center (windows)
  "Cetner windows respecting their sizes."
  )

(defun fullscreen (windows)
  "Make windows fullscreen, ignoring their size hints, EWMH struts.
Don't draw decorations for these windows."
  )

(defun maximize (windows)
  "Maximize windows making them occupy entire working area.
Respects EWMH struts."
  )

(defun iconify (windows)
  
  )
(defun raise (windows)
  "Raise to the top in the stacking order."
  )

(defun focus (windows)
  "Focus matching windows."
  )

(defun prev (windows)
  "Focus previous window.")
