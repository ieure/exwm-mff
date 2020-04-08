;;; exwm-mff.el --- Mouse Follows Focus           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; URL: https://github.com/ieure/exwm-mff
;; Version: 1.0.6
;; Package-Requires: ((emacs "25") (exwm "0.22.1"))
;; Keywords: unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Mouse Follows Focus
;; ===================
;;
;; Traditional window managers are mouse-centric: the window to receive
;; input is usually selected with the pointing device.
;;
;; Emacs is keybord-centric: the window to receive key input is usually
;; selected with the keyboard.  When you use the keyboard to focus a
;; window, the spatial relationship between pointer and active window is
;; broken -- the pointer can be anywhere on the screen, instead of over
;; the active window, which can make it hard to find.
;;
;; (The same problem exists in traditional windowing systems when you use
;; the keyboard to switch windows, e.g. with Alt-Tab.  But we can’t do
;; anything about that here.)
;;
;; Because Emacs’ model is inversed, this suggests that the correct
;; behavior is also the inverse -- instead of using the mouse to select a
;; window to receive keyboard input, the keyboard should be used to
;; select the window to receive mouse input.
;;
;; `EXWM-MFF-MODE' is a global minor mode which does exactly this.  When
;; the selected window in Emacs changes, the mouse pointer is moved to
;; its center, unless the pointer is already somewhere inside the
;; window’s bounds.  It works for both regular Emacs windows and X11
;; clients managed by EXWM.
;;
;; This package also offers the `EXWM-MFF-WARP-TO-SELECTED' command,
;; which allows you to summon the pointer with a hotkey.  Unlike the
;; minor mode, summoning is unconditional, and will place the pointer in
;; the center of the window even if it already resides within its bounds
;; -- a handy feature if you’ve lost your pointer, even if you’re using
;; the minor mode.
;;
;;
;; Limitations
;; ~~~~~~~~~~~
;;
;; Handling of floating frames needs some work; clicking the modeline of
;; a buffer warps the point to the center of the buffer, rather than
;; leaving it where it was when clicked.

;;; Code:

(require 'exwm)
(require 'xelb)

(defvar exwm-mff--debug 0
  "Whether (and how) to debug exwm-mff.
0 = don't debug.
1 = log messages to *exwm-mff-debug*.
2 = log messages to *exwm-mff-debug* and the echo area.")

(defvar exwm-mff--last-window nil
  "The last selected window.")

(defun exwm-mff--guard ()
  "Raise an error unless EXWM is running."
  (unless (eq (window-system) 'x)
    (error "X11 is required to use Exwm-mff-mode"))
  (unless exwm--connection
    (error "EXWM must be running for exwm-mff-mode to work")))

(defun exwm-mff--contains-pointer? (window)
  "Return non-NIL when the mouse pointer is within WINDOW."
  (let ((fp (frame-parameter (selected-frame) 'exwm-outer-id)))
    (if fp
        (with-slots (win-x win-y)
            (xcb:+request-unchecked+reply exwm--connection
                (make-instance 'xcb:QueryPointer
                               :window fp))
          (pcase (window-absolute-pixel-edges window)
            (`(,left ,top ,right ,bottom)
             (and
              (<= left win-x right)
              (<= top win-y bottom)))))
      t)))

(defun exwm-mff--warp-to (window-id x y)
  "Warp the mouse pointer WINDOW-ID, position X, Y."
  (xcb:+request exwm--connection
      (make-instance 'xcb:WarpPointer
                     :src-window xcb:Window:None
                     :dst-window window-id
                     :src-x 0
                     :src-y 0
                     :src-width 0
                     :src-height 0
                     :dst-x x
                     :dst-y y))
  (xcb:flush exwm--connection))

(defun exwm-mff--debug (string &rest objects)
  "Log debug message STRING, using OBJECTS to format it."
  (let ((debug-level (or exwm-mff--debug 0)))
    (when (> debug-level 0)
      (let ((str (apply #'format string objects)))
        (when (>= debug-level 1)
          (with-current-buffer (get-buffer-create " *exwm-mff-debug*")
            (goto-char (point-max))
            (insert (concat str "\n")))
          (when (>= debug-level 2)
            (message str)))))))

(defun exwm-mff--window-center (window)
  "Return a list of (x y) coordinates of the center of WINDOW."
  (pcase (window-absolute-pixel-edges window)
    (`(,left ,top ,right ,bottom)
     (list (+ left (/ (- right left) 2))
           (+ top (/ (- bottom top) 2))))))

(defun exwm-mff-warp-to (window)
  "Place the pointer in the center of WINDOW."
  (apply #'exwm-mff--warp-to exwm--root (exwm-mff--window-center window)))

;;;###autoload
(defun exwm-mff-warp-to-selected ()
  "Place the pointer in the center of the selected window."
  (interactive)
  (exwm-mff--guard)
  (exwm-mff-warp-to (selected-window)))

(defun exwm-mff--explain (same-window? contains-pointer? mini?)
  "Use SAME-WINDOW?, CONTAINS-POINTER? and MINI? to return an explanation of focusing behavior."
  (cond
   (same-window? "selected window hasn't changed")
   (contains-pointer? "already contains pointer")
   (mini? "is minibuffer")
   (t "doesn't contain pointer")))

(defun exwm-mff-hook ()
  "Mouse-Follows-Focus mode hook.

Move the pointer to the currently selected window, if it's not already in it."
  (let* ((sw (selected-window))
         (same-window? (eq sw exwm-mff--last-window))
         (contains-pointer? (exwm-mff--contains-pointer? sw))
         (mini? (minibufferp (window-buffer sw))))
    (if (or same-window? contains-pointer? mini?)
        (exwm-mff--debug "[%s] nop-> %s (%s)"
                         (current-time-string) sw (exwm-mff--explain same-window? contains-pointer? mini?))
      (exwm-mff--debug "[%s] warp-> %s (%s)"
                       (current-time-string) sw (exwm-mff--explain same-window? contains-pointer? mini?))
      (exwm-mff-warp-to (setq exwm-mff--last-window sw)))))

;;;###autoload
(define-minor-mode exwm-mff-mode
  "Mouse follows focus mode for EXWM."
  :global t
  :require 'exwm-mff
  (exwm-mff--guard)
  (if exwm-mff-mode
      (add-hook 'buffer-list-update-hook #'exwm-mff-hook t)
    (remove-hook 'buffer-list-update-hook #'exwm-mff-hook)))

(provide 'exwm-mff)

;;; exwm-mff.el ends here
