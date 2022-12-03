(setq-default gc-cons-threshold 100000000)
(setq warning-minimum-level :emergency
      warning-minimum-log-level :warning)

;; straight =========================================
;; ==================================================

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; Custom Lisp files ================================
;; ==================================================

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions")

;; Korean environment ===============================
;; ==================================================

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<f6>") 'toggle-korean-input-method)
(unbind-key (kbd "C-d"))
(unbind-key (kbd "C-d C-d"))
(unbind-key (kbd "C-d C-l"))
(global-set-key (kbd "C-d C-d") 'toggle-input-method)
(global-set-key (kbd "C-d C-l") 'toggle-input-method)
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; No Littering! ====================================
;; ==================================================

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

;; General.el config ================================
;; ==================================================

(use-package general
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (setq general-use-package-emit-autoloads t)
  (general-create-definer global-leader
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "S-SPC")
  (general-create-definer local-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key
		 (lambda (arg)
		   `(,(cadr (split-string (car arg) " ")) .
		     ,(replace-regexp-in-string "-mode$" ""
						(symbol-name major-mode))))))
  (general-create-definer agnostic-key
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix ""
    "" '(:ignore t))
  (general-create-definer insert-mode-major-mode
    :keymaps 'override
    :states '(insert)
    :prefix "")
  (general-create-definer normal-mode-major-mode
    :keymaps 'override
    :states '(normal visual operator motion)
    :prefix ""))

;; Org config =======================================
;; ==================================================

(use-package org
  :straight nil
  :mode ("\\.org\\'" . org-mode)
  :general
  (local-leader
    :keymaps
    '(org-mode-map)
    "i" '(declare-label "insert")
    "it" 'org-insert-current-time)
  :config
  (defun org-insert-current-time ()
    "insert the curren time at the cursor position."
    (interactive)
    (insert (format-time-string "** %Y-%m-%d %H:%M:%S")))

  (setq org-return-follows-link t
	org-mouse-1-follows-link t
	org-link-descriptive t
	org-hide-emphasis-markers t)
  (evil-define-key 'normal 'org-mode "RET" 'org-open-at-point)
  (add-hook 'org-mode-hook 'org-appear-mode)
  ;; disable auto-pairing of "<" in org-mode
  (add-hook 'org-mode-hook (lambda ()
			     (setq-local electric-pair-inhibit-predicate
					 `(lambda (c)
					    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (setq org-todo-keywords
	'((sequence "TODO" "WORKING" "|" "DONE" "ABORTED"))))

;; Esup config ======================================
;; ==================================================

(use-package esup
  :defer t
  :config
  (setq esup-depth 0))

;; Transient config =================================
;; ==================================================

(use-package transient :defer t)

;; Useful Elisp Libraries ===========================
;; ==================================================

(use-package dash
  :defer t
  :config
  (function-put '-> 'lisp-indent-function nil)
  (function-put '->> 'lisp-indent-function nil)
  (function-put 'if 'lisp-indent-function nil))
(use-package s :defer t)
(use-package ts :defer t)
(defun declare-label (label)
  (list :ignore t :which-key label))
(defmacro plaintext (&rest body)
  (string-join
   (-interpose " "
	       (mapcar (lambda (elem)
			 (cond
			  ((stringp elem) elem)
			  ((symbolp elem) (symbol-name elem)))) body))))

(defmacro comment (&body))
(defun minor-mode-activated-p (minor-mode)
  "Is the given `minor-mode` activated?"
  (let ((activated-minor-modes (mapcar #'car minor-mode-alist)))
    (memq minor-mode activated-minor-modes)))
(defun straight-from-github (package repo)
  ;; TODO: cannot directly invoke in use-package form
  (list package :type 'git :host 'github :repo repo))

;; macOS Settings ===================================
;; ==================================================

(when (memq window-system '(mac ns))
  (setq mac-function-modifier 'hyper
	mac-option-modifier 'meta
	mac-command-modifier 'super))

(agnostic-key
  "s-v" 'yank
  "s-c" 'evil-yank
  "s-x" 'kill-region
  "s-w" 'delete-window
  "s-W" 'delete-frame
  "s-`" 'other-frame
  "s-z" 'undo-tree-undo
  "s-s" 'save-buffer)

;; fix macOS Trash
(when (memq window-system '(mac ns))
  (setq delete-by-moving-to-trash t
	trash-directory "~/.Trash"))

;; Linux Settings ===================================
;; ==================================================



;; evil-mode config =================================
;; ==================================================

(setq evil-undo-system 'undo-tree)
(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-disable-insert-state-bindings t
	evil-want-C-u-scroll t
	evil-want-integration t)
  :config
  (evil-mode 1)
  ;; set leader key in normal state
  ;; (evil-set-leader 'normal (kbd "SPC"))
  ;; set local leader
  ;; (evil-set-leader 'normal "," t)
  (setq evil-motion-state-cursor 'box
	evil-visual-state-cursor 'box
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "Q" 'kill-this-buffer)
  (evil-ex-define-cmd "W" 'save-buffer)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)
  (evil-ex-define-cmd "WQ" 'evil-save-and-close)
  (evil-ex-define-cmd "E" 'evil-edit)
  (setq evil-vsplit-window-right t
	evil-split-window-below t)
  (evil-define-key 'normal 'global (kbd "C-w DEL") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-right)
  (unbind-key (kbd "C-@"))
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)

  (defun evil-toggle-input-method ()
    "when toggle on input method, switch to evil-insert-state if possible.
  when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
    (interactive)
    (if (not current-input-method)
	(if (not (string= evil-state "insert"))
	    (evil-insert-state))
	(if (string= evil-state "insert")
	    (evil-normal-state)))
    (toggle-input-method)))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  (setq evil-collection-calendar-want-org-bindings t))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-anzu
  :after evil)

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-org
  :after evil)

;; Yasnippet config  ================================
;; ==================================================

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; Ripgrep config ===================================
;; ==================================================

(use-package ripgrep :defer t)

;; Tree-sitter config ===============================
;; ==================================================

(use-package tree-sitter
  :hook prog-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;; Eglot config =====================================
;; ==================================================

(use-package eglot
  :hook
  ((rust-mode . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (python-mode . eglot-ensure))
  :general
  (local-leader
    :keymaps
    '(eglot-mode-map)
    "a" '(:ignore t :which-key "LSP")
    "aa" 'eglot-code-actions
    "r" 'eglot-rename)
  :config
  (defvar-local flycheck-eglot-current-errors nil)

  ;; use flycheck instead of flymake.
  (defun flycheck-eglot-report-fn (diags &rest _)
    (setq flycheck-eglot-current-errors
	  (mapcar (lambda (diag)
		    (save-excursion
		      (goto-char (flymake--diag-beg diag))
		      (flycheck-error-new-at (line-number-at-pos)
					     (1+ (- (point) (line-beginning-position)))
					     (pcase (flymake--diag-type diag)
					       ('eglot-error 'error)
					       ('eglot-warning 'warning)
					       ('eglot-note 'info)
					       (_ (error "Unknown diag type, %S" diag)))
					     (flymake--diag-text diag)
					     :checker 'eglot)))
		  diags))
    (flycheck-buffer))

  (defun flycheck-eglot--start (checker callback)
    (funcall callback 'finished flycheck-eglot-current-errors))

  (defun flycheck-eglot--available-p ()
    (bound-and-true-p eglot--managed-mode))

  (flycheck-define-generic-checker 'eglot
    "Report `eglot' diagnostics using `flycheck'."
    :start #'flycheck-eglot--start
    :predicate #'flycheck-eglot--available-p
    :modes '(prog-mode text-mode))

  (push 'eglot flycheck-checkers)

  (defun sanityinc/eglot-prefer-flycheck ()
    (when eglot--managed-mode
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-select-checker 'eglot)
      (flycheck-mode)
      (flymake-mode -1)
      (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn)))

  (add-hook 'eglot--managed-mode-hook 'sanityinc/eglot-prefer-flycheck))

;; Guix config ======================================
;; ==================================================

(use-package guix :defer t)

;; Notify config ====================================
;; ==================================================

(use-package notify
  :straight (notify :type git :host github :repo "tkhoa2711/notify.el"))

;; REPL config ======================================
;; ==================================================

(use-package comint
  :straight nil
  :config
  (normal-mode-major-mode
    "C-j" 'comint-next-input
    "C-k" 'comint-previous-input))

;; Lisp config ======================================
;; ==================================================

(use-package paren
  :straight nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode 1))

(use-package smartparens
  :config
  (smartparens-global-mode)
  ;; disable single quote auto-closing
  (sp-local-pair '(hy-mode fennel-mode clojure-mode lisp-mode emacs-lisp-mode scheme-mode racket-mode lisp-interaction-mode ielm-mode)
		 "'" "'" :actions nil))

(use-package evil-cleverparens
  :hook (hy-mode fennel-mode clojure-mode lisp-mode emacs-lisp-mode scheme-mode racket-mode)
  :after (evil smartparens)
  :init
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  (setq evil-cleverparens-use-additional-bindings t)
  (unless window-system
    (setq evil-cp-additional-bindings (assoc-delete-all "M-[" evil-cp-additional-bindings))
    (setq evil-cp-additional-bindings (assoc-delete-all "M-]" evil-cp-additional-bindings)))
  (evil-cp-set-additional-bindings))

;; kbd-mode config ==================================
;; ==================================================

(use-package kbd-mode
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode")
  :mode "\\.kbd\\'"
  :commands kbd-mode)

;; Elisp config =====================================
;; ==================================================

(use-package elisp-mode
  :straight nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :general
  (local-leader
    :major-modes
    '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps
    '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ef" 'eval-defun
    "er" 'eval-region
    "ee" 'eval-expression
    "ep" 'pp-eval-last-sexp
    "es" 'eval-last-sexp
    "i" 'elisp-index-search))

;; Clojure config ===================================
;; ==================================================

(use-package clojure-mode
  :mode "\\.clj(s|c)?\\'"
  :init
  (setq clojure-indent-style 'align-arguments
	clojure-align-forms-automatically t
	clojure-toplevel-inside-comment-form t))

(use-package cider
  :mode "\\.clj(s|c)?\\'"
  :config
  (setq cider-use-xref nil)
  (setq cider-repl-display-help-banner nil
	cider-repl-buffer-size-limit 100
	cider-pprint-fn 'fipp
	cider-result-overlay-position 'at-point
	cider-overlays-use-font-lock t)
  (defun run-bb ()
    (interactive)
    (if (executable-find "bb")
	(make-comint "babashka" "bb")
	(message "bb not installed")))
  (defun run-nbb ()
    (interactive)
    (if (executable-find "nbb")
	(make-comint "node-babashka" "nbb")
	(message "nbb not installed")))
  (cider-register-cljs-repl-type 'nbb "(+ 1 2 3)")
  (defun mm/cider-connected-hook ()
    (when (eq 'nbb cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))
  (add-hook 'cider-connected-hook #'mm/cider-connected-hook)
  (setq cider-check-cljs-repl-requirements nil)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)
    (use-like-this 'defun)
    (match 'defun)
    (comment 'defun))
  :general
  (local-leader
    :major-modes
    '(clojure-mode t)
    :keymaps
    '(clojure-mode-map)
    "'" 'sesman-start
    "d" '(:ignore t :which-key "debug")
    "db" 'cider-debug-defun-at-point
    "de" 'spacemacs/cider-display-error-buffer
    "dv" '(:ignore t :which-key "inspect values")
    "dve" 'cider-inspect-last-sexp
    "dvf" 'cider-inspect-defun-at-point
    "dvi" 'cider-inspect
    "dvl" 'cider-inspect-last-result
    "dvv" 'cider-inspect-expr
    "e" '(:ignore t :which-key "evaluation")
    "e;" 'cider-eval-defun-to-comment
    "e$" 'spacemacs/cider-eval-sexp-end-of-line
    "e(" 'cider-eval-list-at-point
    "eb" 'cider-eval-buffer
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "ei" 'cider-interrupt
    "el" 'spacemacs/cider-eval-sexp-end-of-line
    "em" 'cider-macroexpand-1
    "eM" 'cider-macroexpand-all
    "ena" 'cider-ns-reload-all
    "enn" 'cider-eval-ns-form
    "enr" 'cider-ns-refresh
    "enl" 'cider-ns-reload
    "ep;" 'cider-pprint-eval-defun-to-comment
    "ep:" 'cider-pprint-eval-last-sexp-to-comment
    "epf" 'cider-pprint-eval-defun-at-point
    "epe" 'cider-pprint-eval-last-sexp
    "er" 'cider-eval-region
    "eu" 'cider-undef
    "ev" 'cider-eval-sexp-at-point
    "eV" 'cider-eval-sexp-up-to-point
    "ew" 'cider-eval-last-sexp-and-replace
    "en" '(:ignore t :which-key "namespace")
    "ena" 'cider-ns-reload-all
    "enn" 'cider-eval-ns-form
    "enr" 'cider-ns-refresh
    "enl" 'cider-ns-reload  ;; SPC u for cider-ns-reload-all
    "ep" '(:ignore t :which-key "pretty print")
    "ep;" 'cider-pprint-eval-defun-to-comment
    "ep:" 'cider-pprint-eval-last-sexp-to-comment
    "epf" 'cider-pprint-eval-defun-at-point
    "epe" 'cider-pprint-eval-last-sexp
    "m" '(:ignore t :which-key "manage repls")
    "mb" 'sesman-browser
    "mi" 'sesman-info
    "mg" 'sesman-goto
    "ms" 'sesman-start
    "ml" '(:ignore t :which-key "link session")
    "mlp" 'sesman-link-with-project
    "mlb" 'sesman-link-with-buffer
    "mld" 'sesman-link-with-directory
    "mlu" 'sesman-unlink
    "mS" '(:ignore t :which-key "sibling sessions")
    "mSj" 'cider-connect-sibling-clj
    "mSs" 'cider-connect-sibling-cljs
    "mq" '(:ignore t :which-key "quit/restart")
    "mqq" 'sesman-quit
    "mqr" 'sesman-restart
    "p" '(:ignore t :which-key "profile")
    "p+" 'cider-profile-samples
    "pc" 'cider-profile-clear
    "pn" 'cider-profile-ns-toggle
    "ps" 'cider-profile-var-summary
    "pS" 'cider-profile-summary
    "pt" 'cider-profile-toggle
    "pv" 'cider-profile-var-profiled-p
    "s" '(:ignore t :which-key "send to repl")
    "sb" 'cider-load-buffer
    "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
    "se" 'spacemacs/cider-send-last-sexp-to-repl
    "sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
    "sf" 'spacemacs/cider-send-function-to-repl
    "sF" 'spacemacs/cider-send-function-to-repl-focus
    "si" 'sesman-start
    "sc" '(:ignore t :which-key "connect external repl")
    "scj" 'cider-connect-clj
    "scm" 'cider-connect-clj&cljs
    "scs" 'cider-connect-cljs
    "sj" '(:ignore t :which-key "jack-in")
    "sjj" 'cider-jack-in-clj
    "sjm" 'cider-jack-in-clj&cljs
    "sjs" 'cider-jack-in-cljs
    "sq" '(:ignore t :which-key "quit/restart repl")
    "sqq" 'cider-quit
    "sqr" 'cider-restart
    "sqn" 'cider-ns-reload
    "sqN" 'cider-ns-reload-all
    "t" '(:ignore t :which-key "test")
    "ta" 'spacemacs/cider-test-run-all-tests
    "tb" 'cider-test-show-report
    "tl" 'spacemacs/cider-test-run-loaded-tests
    "tn" 'spacemacs/cider-test-run-ns-tests
    "tp" 'spacemacs/cider-test-run-project-tests
    "tr" 'spacemacs/cider-test-rerun-failed-tests
    "tt" 'spacemacs/cider-test-run-focused-test
    "=" '(:ignore t :which-key "format")
    "==" 'cider-format-buffer
    "=eb" 'cider-format-edn-buffer
    "=ee" 'cider-format-edn-last-sexp
    "=er" 'cider-format-edn-region
    "=f" 'cider-format-defun
    "g" '(:ignore t :which-key "goto")
    "gb" 'cider-pop-back
    "gc" 'cider-classpath
    "gg" 'spacemacs/clj-find-var
    "gn" 'cider-find-ns
    "h" '(:ignore t :which-key "documentation")
    "ha" 'cider-apropos
    "hc" 'cider-cheatsheet
    "hd" 'cider-clojuredocs
    "hj" 'cider-javadoc
    "hn" 'cider-browse-ns
    "hN" 'cider-browse-ns-all
    "hs" 'cider-browse-spec
    "hS" 'cider-browse-spec-all

    "T" '(:ignore t :which-key "toggle")
    "Te" 'cider-enlighten-mode
					;"Tf" 'spacemacs/cider-toggle-repl-font-locking
					;"Tp" 'spacemacs/cider-toggle-repl-pretty-printing
    "Tt" 'cider-auto-test-mode)
  (global-leader
    "atsb" 'run-bb
    "atsn" 'run-nbb))

;; Hy config ========================================
;; ==================================================

(use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode)
  :general
  (local-leader
    :major-modes
    '(hy-mode inferior-hy-mode t)
    :keymaps
    '(hy-mode-map inferior-hy-mode-map)
    "e" (declare-label "eval")
    "ec" 'hy-shell-eval-current-form
    "er" 'hy-shell-eval-region
    "eb" 'hy-shell-eval-buffer)
  :config
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
      (previous-buffer))))

;; Racket config ====================================
;; ==================================================

(use-package racket-mode
  :mode "\\.rkt\\'"
  :general
  (local-leader
    :major-modes
    '(racket-mode racket-repl-mode racket-xp-mode t)
    :keymaps
    '(racket-mode-map racket-repl-mode-map racket-xp-mode-map)
    ;; errors
    "E" '(:ignore t :which-key "error")
    "En" 'racket-xp-next-error
    "EN" 'racket-xp-previous-error
    ;; navigation
    "g" '(:ignore t :which-key "goto")
    "g`" 'racket-unvisit
    "gg" 'racket-xp-visit-definition
    "gn" 'racket-xp-next-definition
    "gN" 'racket-xp-previous-definition
    "gm" 'racket-visit-module
    "gr" 'racket-open-require-path
    "gu" 'racket-xp-next-use
    "gU" 'racket-xp-previous-use
    ;; doc
    "h" '(:ignore t :which-key "help")
    "ha" 'racket-xp-annotate
    "hd" 'racket-xp-describe
    "hh" 'racket-xp-documentation
    ;; insert
    "i" '(:ignore t :which-key "insert")
    "il" 'racket-insert-lambda
    ;; refactor
    "m" '(:ignore t :which-key "refactor")
    "mr" 'racket-xp-rename
    ;; REPL
    "e" '(:ignore t :which-key "eval")
    "'"  'racket-repl
    "eb" 'racket-run
    "ee" 'racket-send-last-sexp
    "ef" 'racket-send-definition
    "ei" 'racket-repl
    "er" 'racket-send-region
    ;; Tests
    "t" '(:ignore t :which-key "test")
    "tb" 'racket-test))

;; Scheme config ====================================
;; ==================================================

(use-package geiser :mode "\\.scm\\'")
(use-package geiser-chicken :after geiser :defer t)
(use-package geiser-chez :after geiser :defer t)
(use-package geiser-gambit :after geiser :defer t)
(use-package geiser-guile :after geiser :defer t)

;; Haskell config ===================================
;; ==================================================

(use-package haskell-mode
  :mode "\\.(hs|lhs|cabal)"
  :init
  ;; Haskell cabal files interact badly with electric-indent-mode
  (add-hook 'haskell-cabal-mode-hook (lambda ()
				       (when (fboundp 'electric-indent-local-mode)
					 (electric-indent-local-mode -1))))

  (setq
   ;; Use notify.el (if you have it installed) at the end of running
   ;; Cabal commands or generally things worth notifying.
   haskell-notify-p t
   ;; Remove annoying error popups
   haskell-interactive-popup-errors nil
   ;; Better import handling
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t)
  :config
  (defun spacemacs/haskell-interactive-bring ()
    "Bring up the interactive mode for this session without
	 switching to it."
    (interactive)
    (let* ((session (haskell-session))
	   (buffer (haskell-session-interactive-buffer session)))
      (display-buffer buffer)))

  ;; hooks
  (add-hook 'haskell-mode-hook #'spacemacs-haskell//disable-electric-indent)

  ;; prefixes
  (dolist (mode haskell-modes)
    (spacemacs/declare-prefix-for-mode mode "mg" "haskell/navigation")
    (spacemacs/declare-prefix-for-mode mode "ms" "haskell/repl")
    (spacemacs/declare-prefix-for-mode mode "mc" "haskell/cabal")
    (spacemacs/declare-prefix-for-mode mode "mh" "haskell/documentation")
    (spacemacs/declare-prefix-for-mode mode "md" "haskell/debug")
    (spacemacs/declare-prefix-for-mode mode "mr" "haskell/refactor"))
  (spacemacs/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
  (spacemacs/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

  ;; key bindings
  (defun spacemacs/haskell-process-do-type-on-prev-line ()
    (interactive)
    (haskell-process-do-type 1))

  ;; repl key bindings
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-j") 'haskell-interactive-mode-history-next
    (kbd "C-k") 'haskell-interactive-mode-history-previous
    (kbd "C-l") 'haskell-interactive-mode-clear)

  ;; Bind repl
  (spacemacs/register-repl 'haskell
			   'haskell-interactive-switch "haskell")

  (dolist (mode haskell-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
					      "sb"  'haskell-process-load-file
					      "sc"  'haskell-interactive-mode-clear
					      "sS"  'spacemacs/haskell-interactive-bring
					      "ss"  'haskell-interactive-switch
					      "st"  'haskell-session-change-target
					      "'"   'haskell-interactive-switch

					      "ca"  'haskell-process-cabal
					      "cb"  'haskell-process-cabal-build
					      "cc"  'haskell-compile
					      "cv"  'haskell-cabal-visit-file

					      "hd"  'inferior-haskell-find-haddock
					      "hi"  'haskell-process-do-info
					      "ht"  'haskell-process-do-type
					      "hT"  'spacemacs/haskell-process-do-type-on-prev-line

					      "da"  'haskell-debug/abandon
					      "db"  'haskell-debug/break-on-function
					      "dB"  'haskell-debug/delete
					      "dc"  'haskell-debug/continue
					      "dd"  'haskell-debug
					      "dn"  'haskell-debug/next
					      "dN"  'haskell-debug/previous
					      "dp"  'haskell-debug/previous
					      "dr"  'haskell-debug/refresh
					      "ds"  'haskell-debug/step
					      "dt"  'haskell-debug/trace

					      "ri"  'spacemacs/haskell-format-imports)
    (if (eq haskell-completion-backend 'lsp)
	(spacemacs/set-leader-keys-for-major-mode mode
						  "gl"  'haskell-navigate-imports
						  "S"   'haskell-mode-stylish-buffer

						  "hg"  'hoogle
						  "hG"  'haskell-hoogle-lookup-from-local)
	(spacemacs/set-leader-keys-for-major-mode mode
						  "gi"  'haskell-navigate-imports
						  "F"   'haskell-mode-stylish-buffer

						  "hh"  'hoogle
						  "hG"  'haskell-hoogle-lookup-from-local)))

  (evilified-state-evilify-map haskell-debug-mode-map
			       :mode haskell-debug-mode
			       :bindings
			       "RET" 'haskell-debug/select
			       "a" 'haskell-debug/abandon
			       "b" 'haskell-debug/break-on-function
			       "c" 'haskell-debug/continue
			       "d" 'haskell-debug/delete
			       "i" 'haskell-debug/step
			       "s" 'haskell-debug/next
			       "S" 'haskell-debug/previous
			       "r" 'haskell-debug/refresh
			       "t" 'haskell-debug/trace)

  ;; configure C-c C-l so it doesn't throw any errors
  (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
  (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)

  ;; Switch back to editor from REPL
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode
					    "ss"  'haskell-interactive-switch-back)

  ;; Compile
  (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal
					    "C"  'haskell-compile)

  ;; Cabal-file bindings
  (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal-mode
					    ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
					    "d"   'haskell-cabal-add-dependency
					    "b"   'haskell-cabal-goto-benchmark-section
					    "e"   'haskell-cabal-goto-executable-section
					    "t"   'haskell-cabal-goto-test-suite-section
					    "m"   'haskell-cabal-goto-exposed-modules
					    "l"   'haskell-cabal-goto-library-section
					    "n"   'haskell-cabal-next-subsection
					    "p"   'haskell-cabal-previous-subsection
					    "sc"  'haskell-interactive-mode-clear
					    "sS"  'spacemacs/haskell-interactive-bring
					    "ss"  'haskell-interactive-switch
					    "N"   'haskell-cabal-next-section
					    "P"   'haskell-cabal-previous-section
					    "f"   'haskell-cabal-find-or-create-source-file)

  ;; Make "RET" behaviour in REPL saner
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "RET") 'haskell-interactive-mode-return)
  (evil-define-key 'normal haskell-interactive-mode-map
    (kbd "RET") 'haskell-interactive-mode-return)

  ;; align rules for Haskell
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
		 '(haskell-types
		   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
		   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
		 '(haskell-assignment
		   (regexp . "\\(\\s-+\\)=\\s-+")
		   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
		 '(haskell-arrows
		   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
		   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
		 '(haskell-left-arrows
		   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
		   (modes . haskell-modes)))))

(use-package cmm-mode
  :defer t
  :config
  'TODO)

(use-package haskell-snippets
  :when (and (eq major-mode 'haskell-mode)
	     (minor-mode-activated-p 'yas-minor-mode)))

;; Nix config =======================================
;; ==================================================

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-modeline
  :after nix-mode)

(use-package nix-env-install
  :after nix-mode)

(use-package nix-update
  :after nix-mode)

(use-package nix-sandbox
  :after nix-mode)

;; OCaml config =====================================
;; ==================================================

(use-package tuareg
  :bind (:map tuareg-mode-map
	      ;; Workaround to preserve vim backspace in normal mode
	      ([backspace] . nil))
  :mode (("\\.ml[ily]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode))
  :defer t
  :init
  (progn
    (spacemacs//init-ocaml-opam)
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
					      "ga" 'tuareg-find-alternate-file
					      "cc" 'compile)
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))))

(defun ocaml/init-dune ()
  (use-package dune
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
						"tP" 'dune-promote
						"tp" 'dune-runtest-and-promote)
      (spacemacs/declare-prefix-for-mode 'tuareg-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'dune-mode "mc" "compile/check")
      (spacemacs/declare-prefix-for-mode 'dune-mode "mi" "insert-form")
      (spacemacs/declare-prefix-for-mode 'dune-mode "mt" "test")
      (spacemacs/set-leader-keys-for-major-mode 'dune-mode
						"cc" 'compile
						"ia" 'dune-insert-alias-form
						"ic" 'dune-insert-copyfiles-form
						"id" 'dune-insert-ignored-subdirs-form
						"ie" 'dune-insert-executable-form
						"ii" 'dune-insert-install-form
						"il" 'dune-insert-library-form
						"im" 'dune-insert-menhir-form
						"ip" 'dune-insert-ocamllex-form
						"ir" 'dune-insert-rule-form
						"it" 'dune-insert-tests-form
						"iv" 'dune-insert-env-form
						"ix" 'dune-insert-executables-form
						"iy" 'dune-insert-ocamlyacc-form
						"tP" 'dune-promote
						"tp" 'dune-runtest-and-promote))))

(use-package utop
  :defer t
  :init
  (progn
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    (spacemacs/register-repl 'utop 'utop "ocaml"))
  :config
  (progn
    (if (executable-find "opam")
	(setq utop-command "opam config exec -- utop -emacs")
	(spacemacs-buffer/warning "Cannot find \"opam\" executable."))

    (defun spacemacs/utop-eval-phrase-and-go ()
      "Send phrase to REPL and evaluate it and switch to the REPL in
`insert state'"
      (interactive)
      (utop-eval-phrase)
      (utop)
      (evil-insert-state))

    (defun spacemacs/utop-eval-buffer-and-go ()
      "Send buffer to REPL and evaluate it and switch to the REPL in
`insert state'"
      (interactive)
      (utop-eval-buffer)
      (utop)
      (evil-insert-state))

    (defun spacemacs/utop-eval-region-and-go (start end)
      "Send region to REPL and evaluate it and switch to the REPL in
`insert state'"
      (interactive "r")
      (utop-eval-region start end)
      (utop)
      (evil-insert-state))

    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
					      "'"  'utop
					      "sb" 'utop-eval-buffer
					      "sB" 'spacemacs/utop-eval-buffer-and-go
					      "si" 'utop
					      "sp" 'utop-eval-phrase
					      "sP" 'spacemacs/utop-eval-phrase-and-go
					      "sr" 'utop-eval-region
					      "sR" 'spacemacs/utop-eval-region-and-go)
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "ms" "send"))
  (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
  (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev))

;; Rust config ======================================
;; ==================================================

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil)))
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(use-package flycheck-rust
  :after (rust-mode)
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; HTML config ======================================
;; ==================================================

(use-package web-mode
  :mode ("\\.html\\'" . web-mode))

(use-package company-web
  :after (web-mode))

(use-package tagedit
  :after (web-mode))

(use-package emmet-mode
  :after (web-mode))

;; Markdown config ==================================
;; ==================================================

(use-package markdown-mode :mode "\\.md\\'")

;; CSharp config ====================================
;; ==================================================

(use-package csharp-mode :mode "\\.cs\\'")

;; auto-indent on RET ===============================
;; ==================================================

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'prog-mode-hook 'set-newline-and-indent)

;; exec-path-from-shell config ======================
;; ==================================================

(setq explicit-shell-file-name "/bin/zsh")
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables '("JAVA_HOME" "BROWSER" "OPAMCLI")
	exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; hide-mode-line config ============================
;; ==================================================

(use-package hide-mode-line)

;; vertico config ===================================
;; ==================================================

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight nil
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :straight nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(basic substring partial-completion flex orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; marginalia config ================================
;; ==================================================

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; consult config ===================================
;; ==================================================

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("<help> a" . consult-apropos)            ;; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; iedit config =====================================
;; ==================================================

(use-package iedit)

;; transpose-frame config ===========================
;; ==================================================

(use-package transpose-frame :commands (transpose-frame))

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

;; winum configs ===================================
;; =================================================

(use-package winum
  :init (setq winum-auto-setup-mode-line nil)
  :config (winum-mode))

;; scratch buffer configs ==========================
;; =================================================

(setq initial-scratch-message ""
      initial-major-mode 'fundamental-mode)

;; dash configs =====================================
;; ==================================================

(use-package dash
  :defer t
  :config
  (with-eval-after-load 'dash
    (function-put '-> 'lisp-indent-function nil)
    (function-put '->> 'lisp-indent-function nil)))

;; LaTeX config =====================================
;; ==================================================

(use-package tex
  :mode "\\.tex\\'"
  :straight auctex
  :config
  (define-key TeX-mode-map (kbd "s-\\") #'TeX-previous-error)
  (define-key TeX-mode-map (kbd "s-/") #'TeX-next-error)
  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package auctex-lua :after (tex))
(use-package company-auctex :after (company))

;; clipetty config ==================================
;; ==================================================

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

;; company-mode config ==============================
;; ==================================================

(use-package company
  :hook (prog-mode . global-company-mode)
  :commands (company-mode)
  :config
  ;; (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-tooltip-idle-delay 0)
  (setq company-async-redisplay-delay 0)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection))

;; dired configs ====================================
;; ==================================================

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

(use-package xwidget
  :straight nil
  :commands xwidget-new-window
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
  (evil-define-key 'normal xwidget-webkit-mode-map (kbd "s-c") 'xwidget-webkit-copy-selection-as-kill)
  (evil-define-key 'normal xwidget-webkit-mode-map (kbd "q") 'kill-this-buffer)
  (add-hook 'xwidget-webkit-mode-hook
	    (lambda ()
	      (local-unset-key (kbd "<backspace>"))))
  (defun xwidget-webkit-find-file (file)
    (interactive "fFilename: ")
    (xwidget-webkit-new-session (w3m-expand-file-name-as-url file))))

(use-package xwwp :after (xwidget))

;; json config =====================================
;; =================================================

(use-package json-mode :mode "\\.json\\'")

;; kotlin config ===================================
;; =================================================

(use-package kotlin-mode :mode "\\.kt\\'"
  :config
  (defun run-kotlin ()
    (interactive)
    (comint-run "kotlin" '()))
  (local-leader
    :major-modes
    '(kotlin-mode t)
    :keymaps
    '(kotlin-mode-map)
    "'" 'run-kotlin))

;; scala config ====================================
;; =================================================

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; CodeQL config ====================================
;; ==================================================

;; TODO

;; (use-package emacs-codeql
;;   :straight
;;   (emacs-codeql :type git
;; 		:host github
;; 		:repo "anticomputer/emacs-codeql"
;; 		:branch "main")
;;   :after tree-sitter-langs
;;   :demand
;;   :init
;;   (setq codeql-transient-binding "C-c q")
;;   (setq codeql-configure-eglot-lsp t)
;;   (setq codeql-configure-projectile t)
;;   :config
;;   ;; you should configure your standard search paths through a ~/.config/codeql/config entry
;;   ;; e.g. "--search-path /full/path/codeql:/full/path/codeql-go"
;;   ;; see: https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/
;;   ;; this option is here to provide you with load/search precedence control
;;   ;; these paths will have precedence over the config file search paths
;;   (setq codeql-search-paths '("./")))

;; git-gutter config ================================
;; ==================================================

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; magit config =====================================
;; ==================================================

(use-package magit
  :general
  (global-leader
    "g" '(:ignore t :which-key "magit")
    "gs" 'magit
    "ga" 'magit-stage-file
    "gc" 'magit-commit-create
    "gC" 'magit-clone
    "gp" 'magit-push
    "gd" 'magit-diff-dwim)
  :config
  (add-hook 'magit-mode-hook
	    (lambda ()
	      (evil-define-key 'normal magit-mode-map (kbd "SPC") nil))))

;; eldoc-mode config ================================
;; ==================================================

(use-package eldoc
  :straight nil
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . turn-on-eldoc-mode))

;; comments =========================================
;; ==================================================

(agnostic-key "M-;" 'comment-dwim)

;; save-place configs ===============================
;; ==================================================

(use-package saveplace
  :straight nil
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
  (setq which-key-add-column-padding 1
	which-key-allow-multiple-replacements t
	which-key-echo-keystrokes 0.02
	which-key-idle-delay 0.2
	which-key-idle-secondary-delay 0.01
	which-key-max-description-length 32
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-prevent-C-h-from-cycling t
	which-key-sort-order 'which-key-prefix-then-key-order
	which-key-sort-uppercase-first nil
	which-key-special-keys nil
	which-key-use-C-h-for-paging t
	which-key-allow-evil-operators t))

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

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-checker-error-threshold 1000))

;; recentf configs ==================================
;; ==================================================

(use-package recentf
  :straight nil
  :commands (consult-recent-file)
  :init
  (setq recentf-keep '(file-remote-p file-readable-p)
	recentf-save-file (concat user-emacs-directory ".recentf")
	recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 40)
  (add-to-list 'recentf-exclude "/private/var/folders/.*")
  (add-to-list 'recentf-exclude "/var/folders/.*"))

(defun cleanup-emacs ()
  (interactive)
  (garbage-collect)
  (when (featurep 'helpful)
    (helpful-kill-buffers))
  (recentf-cleanup)
  (message "no more garbage! yay!"))

;; ibuffer configs ==================================
;; ==================================================

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile configs ===============================
;; ==================================================

(use-package projectile
  :general
  (global-leader
    "p" '(:ignore t :which-key "project")
    "p/" 'projectile-ripgrep
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pP" 'projectile-switch-open-project
    "pc" 'projectile-compile-project)
  :config
  (projectile-mode)
  (setq projectile-mode-line "Projectile"
	projectile-enable-caching t
	anaconda-mode-localhost-address "localhost"))

;; minions config ===================================
;; ==================================================

(use-package minions
  :config
  (minions-mode 1)
  (setq minions-hidden-modes t))

;; visuals ==========================================
;; ==================================================

(tool-bar-mode -1)
(menu-bar-mode -1)

(defun disable-tab-bar-if-unnecessary (_)
  "Hide the tab bar if there is only one tab left."
  (when (= (length (tab-bar-tabs)) 1)
    (tab-bar-mode -1)))

(advice-add 'tab-close :after #'disable-tab-bar-if-unnecessary)

(fringe-mode '(0 . 0))
(blink-cursor-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil :height 180)

(use-package modus-themes
  :config
  (if (window-system)
      (load-theme 'modus-operandi t)
      (load-theme 'modus-vivendi t))
  (add-hook 'modus-themes-after-load-theme-hook
	    (lambda ()
	      (when (string= (modus-themes--current-theme) "modus-vivendi")
		(set-face-attribute 'fringe nil :background "#000000" :foreground "#000000")))))

(use-package auto-dark
  :if (memq window-system '(mac ns))
  :config
  (setq auto-dark--light-theme 'modus-operandi)
  (setq auto-dark--dark-theme 'modus-vivendi))

(global-visual-line-mode t)

;; make terminal transparent
(unless (window-system)
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))
  (add-hook 'window-setup-hook 'on-after-init))

;; hl-todo config ==================================
;; =================================================

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
	'(("HOLD" . "#d0bf8f")
	  ("TODO" . "#cc9393")
	  ("NEXT" . "#dca3a3")
	  ("THEM" . "#dc8cc3")
	  ("WORKING" . "#7cb8bb")
	  ("PROG" . "#7cb8bb")
	  ("OKAY" . "#7cb8bb")
	  ("DONT" . "#5f7f5f")
	  ("FAIL" . "#8c5353")
	  ("DONE" . "#afd8af")
	  ("NOTE"   . "#d0bf8f")
	  ("KLUDGE" . "#d0bf8f")
	  ("HACK"   . "#d0bf8f")
	  ("TEMP"   . "#d0bf8f")
	  ("FIXME"  . "#cc9393")
	  ("UNSURE"  . "#cc9393")
	  ("XXX+"   . "#cc9393"))))

;; line numbers ====================================
;; =================================================

(let ((hooks '(doc-view-mode-hook
	       pdf-view-mode-hook
	       w3m-mode-hook
	       eww-mode-hook
	       inferior-hy-mode-hook
	       inferior-python-mode-hook
	       vterm-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook
	      (lambda ()
		(display-line-numbers-mode -1)))))

;; vterm config =====================================
;; ==================================================

(use-package vterm)
(use-package multi-vterm
  :after (vterm projectile)
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map (kbd "<return>") #'vterm-send-return)
  (setq vterm-keymap-exceptions nil)
  :config
  (insert-mode-major-mode
    :major-modes
    '(vterm-mode vterm-copy-mode t)
    :keymaps
    '(vterm-mode-map vterm-copy-mode-map)
    "C-e" 'vterm--self-insert
    "C-f" 'vterm--self-insert
    "C-a" 'vterm--self-insert
    "C-v" 'vterm--self-insert
    "C-b" 'vterm--self-insert
    "C-w" 'vterm--self-insert
    "C-u" 'vterm--self-insert
    "C-d" 'vterm--self-insert
    "C-n" 'vterm--self-insert
    "C-m" 'vterm--self-insert
    "C-p" 'vterm--self-insert
    "C-j" 'vterm--self-insert
    "C-k" 'vterm--self-insert
    "C-r" 'vterm--self-insert
    "C-t" 'vterm--self-insert
    "C-g" 'vterm--self-insert
    "C-c" 'vterm--self-insert
    (kbd "C-SPC") 'vterm--self-insert
    "C-d" 'vterm--self-insert)
  (local-leader
    :major-modes
    '(vterm-mode vterm-copy-mode t)
    :keymaps
    '(vterm-mode-map vterm-copy-mode-map)
    "c" 'multi-vterm
    "n" 'multi-vterm-next
    "p" 'multi-vterm-prev)
  (normal-mode-major-mode
    :major-modes
    '(vterm-mode vterm-copy-mode t)
    :keymaps
    '(vterm-mode-map vterm-copy-mode-map)
    "i" 'evil-insert-resume
    "o" 'evil-insert-resume
    (kbd "<return>") #'evil-insert-resume))

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

;; world clock config ===============================
;; ==================================================

(use-package time
  :straight nil
  :config
  (setq world-clock-list t
	zoneinfo-style-world-list '(("America/Los_Angeles" "Los Angeles")
				    ("America/New_York" "New York")
				    ("Asia/Seoul" "Seoul")))
  :general
  (global-leader
    "aC" (declare-label "clock")
    "aCw" 'world-clock))

;; custom functions =================================
;; ==================================================

(defun display-current-time ()
  "display the current time in the buffer."
  (interactive)
  (message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun insert-current-time ()
  "insert the current time at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;; emacs key remappings =============================
;; ==================================================

(agnostic-key
  "C-x C-l" 'count-lines-page)

;; Mode-agnostic keybindings ==========================
;; ====================================================

;; super-shortcuts
(agnostic-key
  "s-1" 'winum-select-window-1
  "s-2" 'winum-select-window-2
  "s-3" 'winum-select-window-3
  "s-4" 'winum-select-window-4
  "s-5" 'winum-select-window-5
  "s-6" 'winum-select-window-6
  "s-7" 'winum-select-window-7
  "s-8" 'winum-select-window-8
  "s-9" 'winum-select-window-9
  "s-0" 'winum-select-window-0)

(agnostic-key
  "s-p" 'projectile-find-file-dwim
  "s-P" 'consult-recent-file
  "s-o" 'find-file
  "s-f" 'projectile-find-file-dwim
  "s-b" 'switch-to-buffer
  "s-e" 'eshell
  "s-{" 'tab-previous
  "s-}" 'tab-next
  "s-[" 'tab-previous
  "s-]" 'tab-next
  "s-." 'tab-new
  "s-," 'tab-close
  "s-;" 'evil-window-vsplit
  "s-'" 'evil-window-split
  "s-h" 'evil-window-left
  "s-j" 'evil-window-down
  "s-k" 'evil-window-up
  "s-l" 'evil-window-right
  "s-u" 'winner-undo
  "s-d" 'kill-this-buffer
  "s-D" 'kill-buffer-and-window
  "s-m" 'helm-filtered-bookmarks
  "s-g" 'magit
  "s-r" 'winner-redo
  "s-i" 'comment-dwim
  "s-t" 'tool-bar-mode
  "s-a" 'org-agenda
  "s-y" 'mu4e-update-mail-and-index
  "s-/" 'flycheck-next-error
  "s-\\" 'flycheck-previous-error
  "s-?" 'yas-next-field
  "s->" 'yas-prev-field)

;; control-super-shortcuts
(agnostic-key
  "C-s-f" 'toggle-frame-fullscreen
  "C-s-v" 'multi-vterm
  "C-s-b" 'ibuffer
  "C-s-/" 'next-error
  "C-s-\\" 'previous-error
  "C-s-=" 'balance-windows
  "C-s-i" 'imenu-list
  "C-s-x" 'xwidget-new-window
  "C-s-." 'hl-todo-occur
  "C-s-;" 'flycheck-previous-error
  "C-s-'" 'flycheck-next-error
  "C-s-e" 'eww
  "C-s-p" 'previous-buffer
  "C-s-n" 'next-buffer
  "C-s-t" 'modus-themes-toggle)

;; leader bindings
(defun visit-init-dot-el ()
  "visit `~/.emacs.d/init.el'."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun eval-init-dot-el ()
  "evaluates the contents of `~/.emacs.d/init.el'."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/init.el")
    (eval-buffer)))

(global-leader
  "SPC" 'execute-extended-command
  "TAB" 'evil-switch-to-windows-last-buffer
  "C-r" 'revert-buffer
  (kbd "x TAB") 'indent-rigidly)

(global-leader
  "w" '(:ignore t :which-key "window")
  "wd" 'delete-window
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wL" 'evil-window-bottom-right

  "wt" 'transpose-frame
  "wr" 'evil-window-rotate-downwards
  "wR" 'evil-window-rotate-upwards

  "w=" 'balance-windows
  "wu" 'winner-undo
  "wU" 'winner-redo
  "w;" 'evil-window-vsplit
  "w'" 'evil-window-split

  ";" 'evil-window-vsplit
  "'" 'evil-window-split

  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "0" 'winum-select-window-0)

(global-leader
  "f" '(:ignore t :which-key "file")
  "ff" 'find-file
  "fs" 'save-buffer
  "fed" 'visit-init-dot-el
  "feR" 'eval-init-dot-el
  "fr" 'consult-recent-file
  "fj" 'dired-jump
  "fF" 'find-name-dired
  "o" 'find-file)

(global-leader
  "b" '(:ignore t :which-key "buffer")
  "bd" 'kill-this-buffer
  "bb" 'switch-to-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer)

(global-leader
  "." 'tab-new
  "," 'tab-close
  "[" 'tab-previous
  "]" 'tab-next
  "/" 'flycheck-next-error
  "\\" 'flycheck-previous-error)

(global-leader
  "a" '(:ignore t :which-key "utilities")
  "ai" 'display-current-time
  "ab" 'battery
  "awm" 'w3m
  "aww" 'eww)

(global-leader
  "q" '(:ignore t :which-key "quit")
  "qq" 'kill-emacs
  "qf" 'delete-frame)

(global-leader
  "h" '(:ignore t :which-key "help")
  "hd" '(:ignore t :which-key "describe")
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdv" 'describe-variable
  "hdm" 'describe-mode)

;; enable mouse scroll in terminal ==================
;; ==================================================

(unless window-system
  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)
  (setq mouse-wheel-up-event 'mouse-5
	mouse-wheel-down-event 'mouse-4))

;; graphviz-dot-mode ================================
;; ==================================================

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; w3m config =======================================
;; ==================================================

(use-package w3m
  :config
  (setq w3m-default-display-inline-images t
	w3m-session-load-crashed-sessions 'never)
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
  (define-key w3m-mode-map (kbd "wc") 'w3m-copy-current-url)
  (evil-define-key 'normal w3m-mode-map (kbd "SPC") nil)
  (defun w3m-open-this-file ()
    (interactive)
    (let ((current-filename (buffer-file-name)))
      (w3m-find-file current-filename))))

;; eww config =======================================
;; ==================================================

(use-package eww
  :straight nil)

;; reddigg config ===================================
;; ==================================================

(use-package reddigg
  :general
  (global-leader
    "awr" '(:ignore t :which-key "reddit")
    "awrm" 'reddigg-view-main
    "awrs" 'reddigg-view-sub)
  :config
  (setq reddigg-subs '(emacs clojure orgmode lisp commandline
			     mechkeyboard scala haskell HHKB clojure
			     vim kotlin programmerhumor orgmode
			     commandline CityPorn OrgRoam))
  (setq org-confirm-elisp-link-function nil))

;; hnreader config ==================================
;; ==================================================

(use-package hnreader
  :general
  (global-leader
    "awh" (declare-label "hackernews")
    "awhn" 'hnreader-news
    "awhp" 'hnreader-past
    "awhN" 'hnreader-newest
    "awha" 'hnreader-ask
    "awhs" 'hnreader-show
    "awhj" 'hnreader-jobs
    "awhb" 'hnreader-best
    "awhm" 'hnreader-more)
  :config
  (setq org-confirm-elisp-link-function nil))

;; eradio config ====================================
;; ==================================================

(use-package eradio
  :general
  (global-leader
    "aR" (declare-label "Radio")
    "aRp" 'eradio-play
    "aRs" 'eradio-stop
    "aRR" 'eradio-toggle)
  :config
  (setq eradio-player '("mpv" "--no-video" "--no-terminal" "--really-quiet")
	eradio-channels '(("MBC FM4U" . "http://serpent0.duckdns.org:8088/mbcfm.pls")
			  ("MBC 표준FM" . "http://serpent0.duckdns.org:8088/mbcsfm.pls")
			  ("KBS 쿨FM" . "http://serpent0.duckdns.org:8088/kbs2fm.pls")
			  ("KBS 해피FM" . "http://serpent0.duckdns.org:8088/kbs2radio.pls")
			  ("KBS 클래식 FM" . "http://serpent0.duckdns.org:8088/kbsfm.pls")
			  ("SBS 파워FM" . "http://serpent0.duckdns.org:8088/sbsfm.pls")
			  ("SBS 러브FM" . "http://serpent0.duckdns.org:8088/sbs2fm.pls")
			  ("TBS 교통방송" . "http://tbs.hscdn.com/tbsradio/fm/playlist.m3u8")
			  ("TBS eFM" . "http://tbs.hscdn.com/tbsradio/efm/playlist.m3u8")
			  ("CBS 음악방송" . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8"))))

;; TRAMP config =====================================
;; ==================================================

(setq tramp-copy-size-limit 10000000
      tramp-inline-compress-start-size 10000000)

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

(evil-define-key 'insert 'global-map (kbd "C-h") 'backward-delete-char)
(evil-define-key 'insert 'company-mode-map (kbd "C-h") 'backward-delete-char)

;; Misc =============================================
;; ==================================================

(use-package sicp :defer t)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message ""
      inhibit-startup-message t
      inhibit-splash-screen t)

(advice-add 'delete-window :after #'balance-windows)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(message "config loaded!")

;; config end =======================================
;; ==================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("a8950f7287870cd993d7e56991a45e1414a09d97e4fbf08f48973a1381bc7aaf" "92d350334df87fe61a682518ff214c773625c6d5ace8060d128adc550bc60c9b" default))
 '(package-selected-packages
   '(no-littering multi-vterm minions xwidget lispy git-gutter clipetty zones yasnippet-classic-snippets treemacs-evil which-key evil-commentary anzu json-mode evil-surround tuareg flycheck tagedit cider))
 '(recentf-auto-cleanup 'never t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#ffffff")))))
(put 'narrow-to-region 'disabled nil)
