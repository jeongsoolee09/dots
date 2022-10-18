(let ((file-name-handler-alist nil))

  (setq gc-cons-threshold 100000000)

  ;; packages =========================================
  ;; ==================================================

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq package-quickstart t)

  ;; Korean environment ===============================
  ;; ==================================================

  (set-language-environment "Korean")
  (prefer-coding-system 'utf-8)
  (global-set-key (kbd "<f6>") 'toggle-korean-input-method)
  (global-set-key (kbd "C-<backspace>") 'toggle-korean-input-method)

  ;; additional packages ==============================
  ;; ==================================================

  (setq my-packages
	'(magit
	  company
	  sicp
	  ripgrep
	  xwwp
	  multi-vterm
	  vterm
	  use-package
	  yasnippet
	  hy-mode
	  pdf-tools
	  transient
	  esup
	  swiper
	  flycheck
	  company
	  undo-tree
	  tuareg
	  git-gutter
	  all-the-icons
	  which-key))

  (when (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  
  ;; Useful Elisp Libraries ===========================
  ;; ==================================================

  (use-package dash)
  (use-package s)
  (use-package ts)
  
  ;; macOS Key Settings ===============================
  ;; ==================================================

  (setq mac-command-modifier 'hyper)
  (setq mac-option-modifier 'meta)

  ;; Lisp config ======================================
  ;; ==================================================
  
  (use-package lispy)

  ;; Clojure config ===================================
  ;; ==================================================

  (use-package clojure-mode)
  (use-package cider)

  ;; HTML config ======================================
  ;; ==================================================

  (use-package tagedit)

  ;; auto-indent on RET ===============================
  ;; ==================================================

  (defun set-newline-and-indent ()
    (local-set-key (kbd "RET") 'newline-and-indent))
  (add-hook 'prog-mode-hook 'set-newline-and-indent)

  ;; exec-path-from-shell config ======================
  ;; ==================================================

  (setq explicit-shell-file-name "/bin/zsh")
  (setq exec-path-from-shell-variables '("JAVA_HOME" "BROWSER" "OPAMCLI"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)

  ;; evil-mode config =================================
  ;; ==================================================

  (setq evil-undo-system 'undo-tree)
  (use-package evil
    :init
    (setq evil-want-C-u-scroll t)
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1)
    ;; set leader key in normal state
    (evil-set-leader 'normal (kbd "SPC"))
    ;; set local leader
    (evil-set-leader 'normal "," t)
    (setq evil-motion-state-cursor 'box)
    (setq evil-visual-state-cursor 'box)
    (setq evil-normal-state-cursor 'box)
    (setq evil-insert-state-cursor 'bar)
    (evil-ex-define-cmd "q" 'kill-this-buffer)
    (evil-ex-define-cmd "Q" 'kill-this-buffer)
    (evil-ex-define-cmd "W" 'save-buffer)
    (evil-ex-define-cmd "Wq" 'evil-save-and-close)
    (evil-ex-define-cmd "WQ" 'evil-save-and-close)
    (evil-ex-define-cmd "E" 'evil-edit)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-define-key 'normal 'global (kbd "C-w DEL") 'evil-window-left)
    (evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-down)
    (evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-up)
    (evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-right)
    (unbind-key (kbd "C-@"))

    (defun evil-toggle-input-method ()
      "when toggle on input method, switch to evil-insert-state if possible.
  when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
      (interactive)
      (if (not current-input-method)
	  (if (not (string= evil-state "insert"))
	      (evil-insert-state))
	(if (string= evil-state "insert")
	    (evil-normal-state)))
      (toggle-input-method))

    (use-package evil-collection
      :after evil
      :config (evil-collection-init))
    
    (use-package evil-surround
      :config
      (global-evil-surround-mode 1))

    (use-package evil-anzu)

    (use-package evil-commentary
      :config (evil-commentary-mode)))
  
    (use-package evil-terminal-cursor-changer)

    (use-package evil-ediff)

  ;; anzu config ======================================
  ;; ==================================================

  (use-package anzu
    :config
    (global-anzu-mode +1))

  ;; transpose-frame config ===========================
  ;; ==================================================

  (use-package transpose-frame)
  
  ;; rainbow delimiters config ========================
  ;; ==================================================

  (use-package rainbow-delimiters
    :hook prog-mode)

  ;; undo-tree config =================================
  ;; ==================================================

  (use-package undo-tree
    :config
    (global-undo-tree-mode)
   (setq undo-tree-auto-save-history nil))

  ;; centered-window-mode config =====================
  ;; =================================================

  (use-package centered-window)

  ;; winum configs ===================================
  ;; =================================================

  (use-package winum
    :init (setq winum-auto-setup-mode-line nil)
    :config (winum-mode))

  ;; scratch buffer configs ==========================
  ;; =================================================

  (setq initial-scratch-message "# 그대 따라갈 이 언덕에\n\n")
  (setq initial-major-mode 'org-mode)

  ;; dash configs =====================================
  ;; ==================================================

  (use-package dash
    :config
    (with-eval-after-load 'dash
      (function-put '-> 'lisp-indent-function nil)
      (function-put '->> 'lisp-indent-function nil)))

  ;; LaTeX config =====================================
  ;; ==================================================

  (use-package tex :ensure auctex)
  (use-package auctex-lua)
  ;; currently broken:
  ; (use-package auctex-latexmk)
  (use-package company-auctex)

  (with-eval-after-load 'tex
    (require 'pdf-sync)
    (define-key LaTeX-mode-map (kbd "H-\\") 'TeX-previous-error)
    (define-key LaTeX-mode-map (kbd "H-/") 'TeX-next-error))
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

  ;; clipetty config ==================================
  ;; ==================================================

  (use-package clipetty
    :hook (after-init . global-clipetty-mode))

  ;; company-mode config ==============================
  ;; ==================================================

  (global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
    (define-key company-active-map (kbd "TAB") #'company-complete-selection))

  ;; eyebrowse config =================================
  ;; ==================================================

  (use-package eyebrowse
    :config
    (eyebrowse-mode))

  ;; dired configs ====================================
  ;; ==================================================

  ;; fix macOS Trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  
  ;; Fix for dired in TRAMP environment
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (when (file-remote-p dired-directory)
		(setq-local dired-actual-switches "-alhB"))))
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (evil-define-key 'normal dired-mode-map (kbd "SPC") nil)))

  ;; xwidget config ===================================
  ;; ==================================================

  (use-package xwwp)
  (use-package xwidget
    :config
    (setq xwidget-webkit-enable-plugins t)
    (defun xwidget-new-window ()
      (interactive)
      (let ((url (read-from-minibuffer "URL: " "https://")))
	(require 's)
	(if (or (s-starts-with-p "https://https://" url)
		(s-starts-with-p "https://http://" url)
		(s-starts-with-p "http://http://" url)
		(s-starts-with-p "http://https://" url))
	    (let ((trimmed (s-chop-prefixes '("https://" "http://") url)))
	      (message (concat "opening " trimmed))
	      (xwidget-webkit-new-session trimmed))
	  (xwidget-webkit-new-session url))))
    (evil-define-key 'normal xwidget-webkit-mode-map (kbd "f") 'xwwp-follow-link)
    (evil-define-key 'normal xwidget-webkit-mode-map (kbd "L") 'xwidget-webkit-browse-url)
    (evil-define-key 'normal xwidget-webkit-mode-map (kbd "H-c") 'xwidget-webkit-copy-selection-as-kill)
    (evil-define-key 'normal xwidget-webkit-mode-map (kbd "q") 'kill-this-buffer)
    (add-hook 'xwidget-webkit-mode-hook
	      (lambda ()
		(local-unset-key (kbd "<backspace>"))))
    (defun xwidget-webkit-find-file (file)
      (interactive "fFilename: ")
      (xwidget-webkit-new-session (w3m-expand-file-name-as-url file))))
  
  ;; json config =====================================
  ;; =================================================

  (use-package json-mode)

  ;; git-gutter config ================================
  ;; ==================================================

  (use-package git-gutter
    :config
    (global-git-gutter-mode +1))

  ;; magit config =====================================
  ;; ==================================================

  (use-package magit
    :config
    (add-hook 'magit-mode-hook
	      (lambda ()
		(evil-define-key 'normal magit-mode-map (kbd "SPC") nil))))

  ;; custom lisp scripts, misc configs ================
  ;; ==================================================

  (add-to-list 'load-path "~/.emacs.d/custom-lisp")
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq create-lockfiles nil)
  (setq electric-indent-mode nil)
  (show-paren-mode 1)

  ;; eldoc-mode config ================================
  ;; ==================================================

  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

  ;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  ;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  ;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  ;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  ;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; comment the current line =========================
  ;; ==================================================

  (global-set-key (kbd "M-;") 'comment-dwim)

  ;; save-place configs ===============================
  ;; ==================================================

  (use-package saveplace
    :config
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "places")))

  ;; winner-mode configs ==============================
  ;; ==================================================

  (winner-mode 1)
  
  ;; which-key configs ================================
  ;; ==================================================

  (use-package which-key
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.3))

  ;; isearch configs ==================================
  ;; ================================================== 

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;; hippie-expand configs ============================
  ;; ================================================== 

  (global-set-key (kbd "M-/") 'hippie-expand)

  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))

  ;; uniquify configs =================================
  ;; ================================================== 

  (setq uniquify-buffer-name-style 'forward)

  ;; flycheck configs =================================
  ;; ================================================== 
  
  (global-flycheck-mode)

  ;; recentf configs ==================================
  ;; ================================================== 

  (use-package recentf
    :init
    (setq recentf-keep '(file-remote-p file-readable-p))
    (setq recentf-save-file (concat user-emacs-directory ".recentf"))
    (setq recentf-auto-cleanup 'never)
    :config
    (recentf-mode 1)
    (setq recentf-max-menu-items 40)
    (add-to-list 'recentf-exclude "/private/var/folders/.*")
    (add-to-list 'recentf-exclude "/var/folders/.*"))

  ;; ibuffer configs ==================================
  ;; ==================================================

  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;; projectile configs ===============================
  ;; ==================================================

  (use-package projectile
    :config
    (projectile-mode))

  ;; visuals ==========================================
  ;; ==================================================

  ;; transparent emacs in terminal
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))

  (add-hook 'window-setup-hook 'on-after-init)

  
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative)

  ;; (defun my-change-window-divider ()
  ;;   (let ((display-table (or buffer-display-table standard-display-table)))
  ;;     (set-display-table-slot display-table 5 ?│)
  ;;     (set-window-display-table (selected-window) display-table)))
  ;; (add-hook 'window-configuration-change-hook 'my-change-window-divider)

  (use-package modus-themes
    :config
    (load-theme 'modus-operandi t))

  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (blink-cursor-mode 0)

  (setq ring-bell-function 'ignore)
  (menu-bar-mode -1)

  (set-face-attribute 'default nil :height 140)
  (setq pdf-view-midnight-colors '("#B0CCDC" . "#000000"))

  ;; line numbers ====================================
  ;; =================================================
  
  (global-visual-line-mode t)
  (setq hooks '(doc-view-mode-hook
		pdf-view-mode-hook
		w3m-mode-hook
		eww-mode-hook
		inferior-hy-mode-hook
		inferior-python-mode-hook
		vterm-mode-hook))
  (dolist (hook hooks)
    (add-hook hook
	      (lambda ()
		(display-line-numbers-mode -1))))


  ;; custom functions =================================
  ;; ==================================================
  
  (global-visual-line-mode t)
  (add-hook 'doc-view-mode-hook
	    (lambda ()
	      (display-line-numbers-mode -1)))
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (display-line-numbers-mode -1)))
  (add-hook 'w3m-mode-hook
	    (lambda ()
	      (display-line-numbers-mode -1)))
  (add-hook 'eww-mode-hook
	    (lambda ()
	      (display-line-numbers-mode -1)))
  (add-hook 'inferior-hy-mode
	    (lambda ()
	      (display-line-numbers-mode -1)))

  ;; org config =======================================
  ;; ==================================================

  (setq org-todo-keywords
	'((sequence "TODO" "WORKING" "|" "DONE" "ABORTED")))

  ;; custom functions =================================
  ;; ==================================================

  (defun display-current-time ()
    "display the current time in the buffer."
    (interactive)
    (message (format-time-string "%Y-%m-%d %H:%M:%S")))

  (defun insert-current-time ()
    "insert the curren time at the cursor position."
    (interactive)
    (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

  (defun org-insert-current-time ()
    "insert the curren time at the cursor position."
    (interactive)
    (insert (format-time-string "** %Y-%m-%d %H:%M:%S")))

  ;; emacs key remappings =============================
  ;; ==================================================
  
  (global-set-key (kbd "C-x C-l") 'count-lines-page)
  ; (global-set-key (kbd "M-x") 'counsel-M-x)

  ;; command-key keybindings ==========================
  ;; ==================================================

  (global-set-key (kbd "H-1") 'winum-select-window-1)
  (global-set-key (kbd "H-2") 'winum-select-window-2)
  (global-set-key (kbd "H-3") 'winum-select-window-3)
  (global-set-key (kbd "H-4") 'winum-select-window-4)
  (global-set-key (kbd "H-5") 'winum-select-window-5)
  (global-set-key (kbd "H-6") 'winum-select-window-6)
  (global-set-key (kbd "H-7") 'winum-select-window-7)
  (global-set-key (kbd "H-8") 'winum-select-window-8)
  (global-set-key (kbd "H-9") 'winum-select-window-9)

  ; (global-set-key (kbd "H-p") 'counsel-recentf)
  (global-set-key (kbd "H-o") 'find-file)
  (global-set-key (kbd "H-f") 'evil-search-forward)
  ; (global-set-key (kbd "H-b") 'counsel-buffer-or-recentf)
  (global-set-key (kbd "H-[") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "H-]") 'eyebrowse-next-window-config)
  (global-set-key (kbd "H-.") 'eyebrowse-create-window-config)
  (global-set-key (kbd "H-,") 'eyebrowse-close-window-config)
  (global-set-key (kbd "H-;") 'evil-window-vsplit)
  (global-set-key (kbd "H-'") 'evil-window-split)
  (global-set-key (kbd "H-h") 'evil-window-left)
  (global-set-key (kbd "H-j") 'evil-window-down)
  (global-set-key (kbd "H-k") 'evil-window-up)
  (global-set-key (kbd "H-l") 'evil-window-right)
  (global-set-key (kbd "H-u") 'winner-undo)
  (global-set-key (kbd "H-d") 'kill-this-buffer)
  ;; (global-set-key (kbd "H-m") 'helm-filtered-bookmarks)
  (global-set-key (kbd "H-g") 'magit)
  (global-set-key (kbd "H-r") 'winner-redo)
  (global-set-key (kbd "H-i") 'comment-dwim)
  (global-set-key (kbd "H-t") 'transpose-frame)
  (global-set-key (kbd "H-a") 'org-agenda)
  (global-set-key (kbd "H-y") 'mu4e-update-mail-and-index)
  (global-set-key (kbd "H-/") 'flycheck-next-error)
  (global-set-key (kbd "H-\\") 'flycheck-previous-error)
  (global-set-key (kbd "H-?") 'yas-next-field)
  (global-set-key (kbd "H->") 'yas-prev-field)

  ;; command-control-shortcuts

  (global-set-key (kbd "C-H-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-H-v") 'multi-vterm)
  (global-set-key (kbd "C-H-b") 'ibuffer)
  (global-set-key (kbd "C-H-/") 'next-error)
  (global-set-key (kbd "C-H-\\") 'previous-error)
  (global-set-key (kbd "C-H-=") 'balance-windows)
  (global-set-key (kbd "C-H-i") 'imenu-list)
  (global-set-key (kbd "C-H-x") 'xwidget-new-window)
  (global-set-key (kbd "C-H-.") 'hl-todo-occur)
  (global-set-key (kbd "C-H-;") 'flycheck-previous-error)
  (global-set-key (kbd "C-H-'") 'flycheck-next-error)

  ;; leader keybindings ===============================
  ;; ==================================================

  (defun visit-init-dot-el ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  ; (evil-define-key 'normal 'global (kbd "<leader>SPC") 'counsel-M-x)
  (evil-define-key 'normal 'global (kbd "<leader>TAB") 'evil-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>x TAB") 'indent-rigidly)

  (evil-define-key 'normal 'global (kbd "<leader>C-r") 'revert-buffer)

  (evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wt") 'transpose-frame)
  (evil-define-key 'normal 'global (kbd "<leader>wr") 'evil-window-rotate-downwards)
  (evil-define-key 'normal 'global (kbd "<leader>w=") 'balance-windows)
  (evil-define-key 'normal 'global (kbd "<leader>wu") 'winner-undo)
  (evil-define-key 'normal 'global (kbd "<leader>wU") 'winner-redo)
  (evil-define-key 'normal 'global (kbd "<leader>w;") 'evil-window-vsplit)
  (evil-define-key 'normal 'global (kbd "<leader>w'") 'evil-window-split)

  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fed") 'visit-init-dot-el)
  ; (evil-define-key 'normal 'global (kbd "<leader>fr") 'counsel-recentf)
  (evil-define-key 'normal 'global (kbd "<leader>fd") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fj") 'dired-jump)

  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>ga") 'magit-stage-file)
  (evil-define-key 'normal 'global (kbd "<leader>gc") 'magit-commit-create)
  (evil-define-key 'normal 'global (kbd "<leader>gp") 'magit-push)

  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)

  (evil-define-key 'normal 'global (kbd "<leader>1") 'winum-select-window-1)
  (evil-define-key 'normal 'global (kbd "<leader>2") 'winum-select-window-2)
  (evil-define-key 'normal 'global (kbd "<leader>3") 'winum-select-window-3)
  (evil-define-key 'normal 'global (kbd "<leader>4") 'winum-select-window-4)
  (evil-define-key 'normal 'global (kbd "<leader>5") 'winum-select-window-5)
  (evil-define-key 'normal 'global (kbd "<leader>6") 'winum-select-window-6)
  (evil-define-key 'normal 'global (kbd "<leader>7") 'winum-select-window-7)
  (evil-define-key 'normal 'global (kbd "<leader>8") 'winum-select-window-8)
  (evil-define-key 'normal 'global (kbd "<leader>9") 'winum-select-window-9)


  (evil-define-key 'normal 'global (kbd "<leader>.") 'eyebrowse-create-window-config)
  (evil-define-key 'normal 'global (kbd "<leader>,") 'eyebrowse-close-window-config)
  (evil-define-key 'normal 'global (kbd "<leader>[") 'eyebrowse-prev-window-config)
  (evil-define-key 'normal 'global (kbd "<leader>]") 'eyebrowse-next-window-config)
  (evil-define-key 'normal 'global (kbd "<leader>;") 'evil-window-vsplit)
  (evil-define-key 'normal 'global (kbd "<leader>'") 'evil-window-split)
  ; (evil-define-key 'normal 'global (kbd "<leader>o") 'counsel-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>/") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "<leader>\\") 'flycheck-previous-error)

  (evil-define-key 'normal 'global (kbd "<leader>it") 'org-insert-current-time)

  (evil-define-key 'normal 'global (kbd "<leader>ai") 'display-current-time)
  (evil-define-key 'normal 'global (kbd "<leader>awm") 'w3m)
  (evil-define-key 'normal 'global (kbd "<leader>aww") 'eww)

  (evil-define-key 'normal 'global (kbd "<leader>p/") 'projectile-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
  
  (evil-define-key 'normal 'global (kbd "<leader>cC") 'compile)
  
  (evil-define-key 'normal 'global (kbd "<leader>qq") 'kill-emacs)
  (evil-define-key 'normal 'global (kbd "<leader>qf") 'delete-frame)

  ;; enable mouse scroll in terminal ==================
  ;; ==================================================

  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
    (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)
    (setq mouse-wheel-up-event 'mouse-5)
    (setq mouse-wheel-down-event 'mouse-4))

  ;; graphviz-dot-mode ================================
  ;; ==================================================

  (use-package graphviz-dot-mode
    :config
    (setq graphviz-dot-indent-width 4))

  ;; w3m config =======================================
  ;; ==================================================

  (use-package w3m
    :config
    (setq w3m-default-display-inline-images t)
    (setq w3m-session-load-crashed-sessions 'never)
    (defun xwidget-webkit-open-w3m-current-url ()
      (interactive)
      (require 'xwidget)
      (xwidget-webkit-new-session w3m-current-url))
    (defun eww-open-w3m-current-url ()
      (interactive)
      (eww-browse-url w3m-current-url))
    (evil-define-key 'normal 'global (kbd "<leader>awx") 'xwidget-webkit-open-w3m-current-url)
    (evil-define-key 'normal 'global (kbd "<leader>awW") 'eww-open-w3m-current-url)
    (setq w3m-search-word-at-point nil)
    (if window-system
	(setq browse-url-browser-function 'browse-url-default-browser)
      (setq browse-url-browser-function 'w3m-browse-url))
    (defun w3m-copy-current-url ()
      (interactive)
      (kill-new w3m-current-url)
      (message "Copied current URL."))
    (eval-after-load 'w3m
      '(progn
	 (define-key w3m-mode-map (kbd "wc") 'w3m-copy-current-url)
	 (evil-define-key 'normal w3m-mode-map (kbd "SPC") nil)))
    (defun w3m-open-this-file ()
      (interactive)
      (let ((current-filename (buffer-file-name)))
	(w3m-find-file current-filename))))

  ;; all-the-icons config =============================
  ;; ==================================================

  (when window-system
    (use-package all-the-icons
      :config (setq all-the-icons-scale-factor 1.0)))

  ;; hy config ========================================
  ;; ==================================================

  (use-package smartparens
    :config (sp-local-pair '(hy-mode) "'" "'" :actions nil)) ; disable single quote auto-closing
  (evil-define-key 'normal hy-mode-map (kbd "<leader>ks") 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal hy-mode-map (kbd "<leader>kb") 'paredit-forward-barf-sexp)
  (evil-define-key 'normal inferior-hy-mode-map (kbd "<leader>ks") 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal inferior-hy-mode-map (kbd "<leader>kb") 'paredit-forward-barf-sexp)

  (defun my-hy-shell-eval-current-form ()
    (interactive)
    (progn
      (hy-shell-eval-current-form)
      (previous-buffer)))

  (defun my-hy-shell-eval-region ()
    (interactive)
    (progn
      (hy-shell-eval-region)
      (previous-buffer)))

  (defun my-hy-shell-eval-buffer ()
    (interactive)
    (progn
      (hy-shell-eval-buffer)
      (previous-buffer)))

  (evil-define-key 'normal hy-mode-map (kbd "<localleader>ec") 'hy-shell-eval-current-form)
  (evil-define-key 'normal hy-mode-map (kbd "<localleader>er") 'hy-shell-eval-region)
  (evil-define-key 'normal hy-mode-map (kbd "<localleader>eb") 'hy-shell-eval-buffer)

  (add-hook 'hy-mode-hook 'paredit-mode)
  (add-hook 'inferior-hy-mode-hook 'paredit-mode)

  ;; TRAMP config =====================================
  ;; ==================================================

  (setq tramp-copy-size-limit 10000000)
  (setq tramp-inline-compress-start-size 10000000)

  (with-eval-after-load 'git-gutter+
    (defun git-gutter+-remote-default-directory (dir file)
      (let* ((vec (tramp-dissect-file-name file))
	     (method (tramp-file-name-method vec))
	     (user (tramp-file-name-user vec))
	     (domain (tramp-file-name-domain vec))
	     (host (tramp-file-name-host vec))
	     (port (tramp-file-name-port vec)))
	(tramp-make-tramp-file-name method user domain host port dir)))

    (defun git-gutter+-remote-file-path (dir file)
      (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
	(replace-regexp-in-string (concat "\\`" dir) "" file))))

  ;; killing ==========================================
  ;; ==================================================

  (setq select-enable-clipboard t
	select-enable-primary t
	save-interprogram-paste-before-kill t
	apropos-do-all t
	mouse-yank-at-point t)

  (set-face-attribute 'default nil :height 140)

  (message "config loaded!")
  ;; config end =======================================
  ;; ==================================================
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("a8950f7287870cd993d7e56991a45e1414a09d97e4fbf08f48973a1381bc7aaf" "92d350334df87fe61a682518ff214c773625c6d5ace8060d128adc550bc60c9b" default))
 '(package-selected-packages
   '(xwidget lispy merlin-iedit iedit git-gutter clipetty zones yasnippet-classic-snippets treemacs-evil which-key evil-commentary anzu json-mode evil-surround tuareg flycheck tagedit cider))
 '(recentf-auto-cleanup 'never))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#ffffff")))))
