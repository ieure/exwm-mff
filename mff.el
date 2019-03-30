
(defvar mff--last-focused-window nil)

(defun mff--warp-to (window-id x y)
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

(defun mff--warp-to-exwm-buffer (exwm-buffer)
  (with-current-buffer exwm-buffer
    (exwm-manage--update-geometry exwm--id t)
    (mff--warp-to exwm--id
                  (/ (slot-value exwm--geometry 'width) 2)
                  (/ (slot-value exwm--geometry 'height) 2))))

(defun mff--warp-to-plain-buffer (buffer)
  (with-current-buffer buffer
    (pcase (window-absolute-pixel-edges (get-buffer-window buffer))
      (`(,left ,top ,right ,bottom)
       (mff--warp-to exwm--root
                     (+ left (/ (- right left) 2))
                     (+ top (/ (- bottom top) 2)))))))

(defun mff-warp (buffer)
  "Place the pointer in the EXWM window BUFFER."
  (funcall (if exwm--id #'mff--warp-to-exwm-buffer #'mff--warp-to-plain-buffer)
           buffer)
  (setq mff--last-focused-window (get-buffer-window buffer)))

(defun mff-hook ()
  (let ((cw (selected-window)))
    (unless (eq mff--last-focused-window cw)
      ;; (message "Warping to buffer %s (last warped %s)" cw mff--last-focused-window)
      (mff-warp (current-buffer)))))

(add-hook 'buffer-list-update-hook #'mff-hook t)


