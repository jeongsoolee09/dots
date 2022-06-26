(require 's)

(setq *org-babel-interpreter-buffer-name* nil)

(defun needs-redefinition? ()
  (or (null *org-babel-interpreter-buffer-name*)
      (not (-contains? (mapcar #'buffer-name (buffer-list))
                       *org-babel-interpreter-buffer-name*))))


(defun prompt-for-buffer-name ()
  (interactive)
  (setq *org-babel-interpreter-buffer-name* (read-from-minibuffer "Buffer name: " "*vterminal<>*")))

(setq mode-and-delimiters '(("tuareg" . ";;\n") ("elisp" . "\n")))

(defun find-mode-delimiter (mode-string)
  (cdr (assoc mode-string mode-and-delimiters #'equal)))

(defun org-babel-repl-send-block ()
  (interactive)
  (when (needs-redefinition?) (prompt-for-buffer-name))
  (let* ((mode (car (org-babel-get-src-block-info)))
         (raw-string (cadr (org-babel-get-src-block-info)))
         (mode-delimiter (find-mode-delimiter mode)))
    (comint-send-string *org-babel-interpreter-buffer-name* (concat raw-string mode-delimiter))))

(defun org-babel-repl-send-region (start end)
  (interactive "r")
  (when (needs-redefinition?) (prompt-for-buffer-name))
  (let* ((mode (car (org-babel-get-src-block-info)))
         (raw-string (buffer-substring-no-properties start end))
         (mode-delimiter (find-mode-delimiter mode)))
    (comint-send-string *org-babel-interpreter-buffer-name* (concat raw-string mode-delimiter))))

(defun org-babel-repl-send-line ()
  (interactive)
  (when (needs-redefinition?) (prompt-for-buffer-name))
  (let* ((mode (car (org-babel-get-src-block-info)))
         (raw-string (thing-at-point 'line t))
         (mode-delimiter (find-mode-delimiter mode)))
    (comint-send-string *org-babel-interpreter-buffer-name* (concat raw-string mode-delimiter))))

(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

(defun org-babel-repl-send-file ()
  "tangle the buffer then eval the buffer's string."
  (interactive)
  (when (needs-redefinition?) (prompt-for-buffer-name))
  (let ((this-filename (buffer-file-name)))
    (progn
      (let ((target-filename (car (org-babel-tangle-file this-filename))))
        (find-file target-filename)
        (let ((raw-string (buffer-substring-no-properties (point-min) (point-max)))
              (mode-delimiter (find-mode-delimiter "tuareg")))
          (comint-send-string *org-babel-interpreter-buffer-name* (concat raw-string mode-delimiter))))
      (previous-buffer))))

(spacemacs/declare-prefix-for-mode 'org-mode "v" "org-babel-repl")
(spacemacs/set-leader-keys-for-minor-mode 'org-mode "vb" #'org-babel-repl-send-block)
(spacemacs/set-leader-keys-for-minor-mode 'org-mode "vr" #'org-babel-repl-send-region)
(spacemacs/set-leader-keys-for-minor-mode 'org-mode "vl" #'org-babel-repl-send-line)
(spacemacs/set-leader-keys-for-minor-mode 'org-mode "vf" #'org-babel-repl-send-file)

(provide 'org-babel-repl)
