(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

;; Move text up and down
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(defun back-window ()
  (interactive)
  (other-window -1))

;; Change panes in GUI
(global-set-key (kbd "C-{") 'back-window)
(global-set-key (kbd "C-}") 'other-window)

;; Escreen settings
(add-hook 'escreen-goto-screen-hook
		  'escreen-enable-number-mode-if-more-than-one-screen)
(global-set-key (kbd "C-c 0") 'escreen-goto-screen-0)
(global-set-key (kbd "C-c 1") 'escreen-goto-screen-1)
(global-set-key (kbd "C-c 2") 'escreen-goto-screen-2)
(global-set-key (kbd "C-c 3") 'escreen-goto-screen-3)
(global-set-key (kbd "C-c 4") 'escreen-goto-screen-4)
(global-set-key (kbd "C-c 5") 'escreen-goto-screen-5)
(global-set-key (kbd "C-c 6") 'escreen-goto-screen-6)
(global-set-key (kbd "C-c 7") 'escreen-goto-screen-7)
(global-set-key (kbd "C-c 8") 'escreen-goto-screen-8)
(global-set-key (kbd "C-c 9") 'escreen-goto-screen-9)
(global-set-key (kbd "C-c >") 'escreen-goto-next-screen)
(global-set-key (kbd "C-c <") 'escreen-goto-prev-screen)