;;; exwm-mff.el --- Mouse Follows Focus           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; URL: https://github.com/ieure/exwm-mff
;; Version: 1.0.1
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

;; Many traditional window managers offer a focus-follows-mouse mode,
;; where moving the pointer over a window gives it input focus, without
;; the need to click.
;;
;; With Emacs' keyboard-centric interaction, windows are typically
;; selected with the keyboard.  However, using the keyboard breaks the
;; spatial relationship between focused window and mouse pointer, which
;; makes it harder to find the pointer location when you need to use
;; it.
;;
;; The appropriate focusing model for this is the inverse of
;; focus-follows-mouse -- instead of using4 the mouse to select the
;; window to receive keyboard input, the keyboard should be used to
;; select the window to receive mouse input.
;;
;; EXWM-MFF-MODE is a global minor mode which does exactly that.
;; When the selected window in Emacs changes, the mouse pointer is
;; moved to the center of it (unless the pointer is already within the
;; window’s bounds).  It works for both regular Emacs windows
;; and ones managed by EXWM.
;;
;; If you don't like the automatic behavior, you can bind
;; EXWM-MFF-WARP-TO-SELECTED-WINDOW which allows you to summon the
;; pointer with a hotkey.

;;; Code:

(require 'exwm)
(require 'xelb)

(defvar exwm-mff--last-focused-window nil
  "The last window exwm-mff warped to.")

(defun exwm-mff--contains-pointer? (window)
  "Returns non-NIL when the mouse pointer is within WINDOW?"
  (with-slots (win-x win-y)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance 'xcb:QueryPointer
                         :window (frame-parameter (selected-frame)
                                                  'exwm-outer-id)))
    (pcase (window-absolute-pixel-edges window)
      (`(,left ,top ,right ,bottom)
       (and
        (<= left win-x right)
        (<= top win-y bottom))))))

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

(defun exwm-mff--window-center (window)
  "Return a list of (x y) coordinates of WINDOW."
  (pcase (window-absolute-pixel-edges window)
    (`(,left ,top ,right ,bottom)
     (list (+ left (/ (- right left) 2))
           (+ top (/ (- bottom top) 2))))))

(defun exwm-mff--warp-to-window (window)
  "Place the mouse pointer in the center of WINDOW."
  (apply #'exwm-mff--warp-to exwm--root (exwm-mff--window-center window)))

;;;###autoload
(defun exwm-mff-warp (window)
  "Place the pointer in the center of WINDOW."
  (exwm-mff--warp-to-window window)
  (setq exwm-mff--last-focused-window window))

(defun exwm-mff-warp-to-selected-window ()
  "Move the pointer to the selected window."
  (interactive)
  (exwm-mff-warp (selected-window)))

(defun exwm-mff-hook ()
  (interactive)
  "Mouse-Follows-Focus mode hook.

Move the pointer to the currently selected window, if it's not already in it."
  (let* ((sw (selected-window))
        (contains? (exwm-mff--contains-pointer? sw)))
    ;; (message "Selecting window %s, contains pointer? %s" sw contains?)

    (unless (or contains? (minibufferp (window-buffer sw)))
      (exwm-mff-warp sw))))

;;;###autoload
(define-minor-mode exwm-mff-mode
  "Mouse follows focus mode for EXWM."
  :global t
  (if exwm-mff-mode
      (add-hook 'buffer-list-update-hook #'exwm-mff-hook t)
    (remove-hook 'buffer-list-update-hook #'exwm-mff-hook)))

(provide 'exwm-mff)

;;; exwm-mff.el ends here
