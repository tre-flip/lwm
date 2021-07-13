;; keys.lisp

;; Contains functions and special variables for reading keyboard and mouse input.

(in-package :lwm)

(defparameter *modkey-state-mask* (xlib:make-state-mask :mod-4))


(defun char->code (char-or-str)
  (car (last (multiple-value-list
	      (xlib:keysym->keycodes (display)
				     (car (xlib:character->keysyms (if (stringp char-or-str)
								       (aref char-or-str 0)
								       char-or-str))))))))

(defun resolve-key (state code &optional window)
  ;; let's ignore WINDOW for a while
  (declare (ignore window))
  (print "ENTER RESOLVE-KEY")
  (when (equal state *modkey-state-mask*)
    (cond
      ((equal code (char->code "t"))
       (uiop:launch-program "xterm"))
      ((equal code (char->code "d"))
       (uiop:launch-program "dmenu_run"))
      ((equal code (char->code "c"))
       (move (current-window)
	     :dx 10)))))

;; TODO: docstring
(defun grab-super+char (name)
  "Grab keys that are defined in ..."
  (xlib:grab-key (root)
		 (char->code name)
		 :modifiers '(:mod-4)))
		 
;; TODO: docstring
(defun grab-keybindings ()
  "Grab that are defined in ..."
  (grab-super+char "d")
  (grab-super+char "c")
  (grab-super+char "t"))

;; TODO: docstring
(defun ungrab-keybindings ()
  "Ungrab that are defined in ..."
  (xlib:ungrab-key (root) :any))
