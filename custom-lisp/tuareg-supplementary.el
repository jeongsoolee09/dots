(require 's)
(require 'tuareg)
(require 'emamux)

(setq *interpreter-buffer-name* nil)

(defun needs-redefinition? ()
  (or (null *interpreter-buffer-name*)
      (not (-contains? (mapcar #'buffer-name (buffer-list))
                       *interpreter-buffer-name*))))


(defun prompt-for-buffer-name ()
  (interactive)
  (setq *interpreter-buffer-name* (read-from-minibuffer "Buffer name: " "*vterminal<>*")))


(defun term-use-this-buffer ()
  (interactive)
  (comint-send-string *interpreter-buffer-name* (concat "#use \"" buffer-file-name "\";;\n")))


(defun term-eval-phrase ()
  (interactive)
  (let ((end))
    (save-excursion
      (let ((triple (funcall utop-discover-phrase)))
        (setq end (nth 2 triple))
        (let ((raw-string (buffer-substring-no-properties
                           (nth 0 triple) (nth 1 triple))))
          (comint-send-string *interpreter-buffer-name* (concat raw-string ";;\n")))))))


(defun term-eval-region (start end)
  (interactive "r")
  (let ((raw-string (buffer-substring-no-properties start end)))
    (comint-send-string *interpreter-buffer-name* (concat raw-string ";;\n"))))


(defun term-eval-line ()
  (interactive)
  (let* ((start (point-at-bol))
         (end (point-at-eol))
         (raw-string (buffer-substring-no-properties start end)))
    (comint-send-string *interpreter-buffer-name*
                        (concat (s-chop-suffix " in" (s-trim raw-string)) ";;\n"))))


(defun term-eval-buffer ()
  (interactive)
  (let ((raw-string (buffer-substring-no-properties (point-min) (point-max))))
    (comint-send-string *interpreter-buffer-name* (concat raw-string ";;\n"))))

;; ================================================
(defun tmux-setup-for-utop ()
  (interactive)
  (when (not (emamux:set-parameters-p))
    (emamux:set-parameters))
  (emamux:send-command "dune build; dune utop" (emamux:target-session)))


(defun tmux-eval-phrase ()
  (interactive)
  (let ((end))
    (save-excursion
      (let ((triple (funcall utop-discover-phrase)))
        (setq end (nth 2 triple))
        (let ((raw-string (buffer-substring-no-properties
                           (nth 0 triple) (nth 1 triple))))
          (emamux:send-command (concat raw-string ";;") (emamux:target-session)))))))


(defun tmux-eval-region (start end)
  (interactive "r")
  (let ((raw-string (buffer-substring-no-properties start end)))
    (emamux:send-command (concat raw-string ";;") (emamux:target-session))))


(defun tmux-eval-line ()
  (interactive)
  (let* ((start (point-at-bol))
         (end (point-at-eol))
         (raw-string (buffer-substring-no-properties start end)))
    (emamux:send-command (concat (s-chop-suffix " in" (s-trim raw-string)) ";;") (emamux:target-session))))


(defun tmux-eval-buffer ()
  (interactive)
  (let ((raw-string (buffer-substring-no-properties (point-min) (point-max))))
    (emamux:send-command (concat raw-string ";;") (emamux:target-session))))

(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "vm" #'prompt-for-buffer-name)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "vp" #'term-eval-phrase)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "vr" #'term-eval-region)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "vb" #'term-eval-buffer)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "vl" #'term-eval-line)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "vu" #'term-use-this-buffer)

(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "ll" #'tmux-eval-phrase)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "lr" #'tmux-eval-region)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "lb" #'tmux-eval-buffer)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "lL" #'tmux-eval-line)
(spacemacs/set-leader-keys-for-minor-mode 'tuareg-mode "lm" #'tmux-setup-for-utop)

(provide 'tuareg-supplementary)
