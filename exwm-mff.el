;;; exwm-mff.el --- Mouse Follows Focus           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; URL: https://github.com/ieure/exwm-mff
;; Version: 1.0.0
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

;; Many mouse-centric window managers offer a "focus follows mouse"
;; mode, where any window the pointer enters receives focus
;; automatically, without clicking to select it.
;;
;; With Emacs' keyboard-centric interaction, windows typically aren't
;; selected with the mouse.  If you subsequently need to interact with
;; the X client using a pointing device, it can be frustrating to find
;; the last place you left it.
;;
;; The appropriate model is the inverse of focus follows mouse, where
;; the pointer is moved to the selected window when it becomes active:
;; mouse follows focus.  Which is what this does.
;;
;; exwm-mff-mode is a global minor mode which places the pointer in
;; the center of the selected Emacs window when focus changes.  It
;; works for both regular Emacs windows and ones managed by EXWM.
;;
;; If you don't like the automatic behavior, you can bind
;; EXWM-MFF-SUMMON which allows you to summon the
;; pointer with a hotkey.
;;

;;; Code:

(require 'exwm)
(require 'xelb)

(defvar exwm-mff--last-focused-window nil
  "The last window exwm-mff warped to.")

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

Move the pointer to the currently selected window, but only if
the selection has changed since the last warp."
  (let ((sw (selected-window)))
    (unless (eq exwm-mff--last-focused-window sw)
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
