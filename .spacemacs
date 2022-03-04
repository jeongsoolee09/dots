;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path nil
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((lsp :variables lsp-lens-enable nil
          auto-completion-idle-delay 0)
     (auto-completion :variables company-mode-completion-cancel-keywords nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior nil)
     syntax-checking
     (elm :variables elm-backend 'lsp)
     spacemacs-modeline
     spacemacs-layouts
     emacs-lisp
     (fsharp :variables fsharp-backend 'lsp)
     (javascript :variables javascript-backend 'lsp)
     (clojure :variables clojure-backend 'lsp)
     (html :variables css-enable-lsp t
           less-enable-lsp t
           scss-enable-lsp t
           html-enable-lsp t)
     helm
     git
     (markdown :variables markdown-live-preview-engine 'vmd)
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'vterm)
     (haskell :variables haskell-backend 'lsp)
     (ocaml :variables ocaml-backend 'lsp)
     osx
     coq
     (python :variables python-backend 'lsp
             python-lsp-server 'mspyls)
     pdf
     racket
     (java :variables java-backend 'lsp)
     graphviz
     csv
     ipython-notebook
     common-lisp
     hy
     (elfeed :variables rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
     scheme
     latex
     ibuffer
     epub
     w3m
     hackernews
     (scala :variables scala-backend 'lsp)
     (ruby :variables ruby-backend 'lsp)
     xclipboard
     (perl5 :variables perl5-backend 'lsp))
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(transpose-frame emms sicp s ts multi-vterm eradio xwwp)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(meghanada anaconda-mode merlin)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner (concat spacemacs-banner-directory "img/skull.png")
   ;; dotspacemacs-startup-banner (concat spacemacs-banner-directory "img/resistance_logo.png")
   ;; dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((agenda . 6)
                                (todos . 6))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '((tron-legacy :location local))
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 18.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "S-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; I don't know, maybe spacemacs uses this in develop
   dotspacemacs-mode-line-theme '(spacemacs :separator slant :separator-scale 1.3)
   ;; dotspacemacs-mode-line-theme '(all-the-icons :separator slant :separator-scale 1.8)
   ;; dotspacemacs-mode-line-theme '(doom :separator-scale 1.0)
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;; :disabled-for-modes dired-mode
   ;; doc-view-mode
   ;; markdown-mode
   ;; org-mode
   ;; pdf-view-mode
   ;; :size-limit-kb 1000)
   ;; (default nil)
   ;; dotspacemacs-line-numbers '(:relative t
   ;;                                       :disabled-for-modes dired-mode
   ;;                                       doc-view-mode
   ;;                                       pdf-view-mode
   ;;                                       spacemacs-buffer-mode)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))

  (setq explicit-shell-file-name "/bin/zsh"
        shell-file-name "/bin/zsh")

  ;; Fix for dired in TRAMP environment
  (add-hook 'dired-mode-hook
            (lambda ()
              (when (file-remote-p dired-directory)
                (setq-local dired-actual-switches "-alhB"))))

  (setq dotspacemacs-scroll-bar-while-scrolling nil)

  (setq warning-minimum-level :emergency
        warning-minimum-log-level :emergency
        native-comp-async-report-warnings-errors nil)

  ) ;; user-init end

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; directories for custom lisp files
  ;; NOTE PUT THESE LINES ON TOP
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (add-to-list 'load-path "~/.emacs.d/private/local/custom-lisp/")

  ;; transparent emacs in terminal
  (when (not window-system)
    (defun on-after-init ()
      (unless (display-graphic-p (selected-frame))
        (set-face-background 'default "unspecified-bg" (selected-frame))))

    (add-hook 'window-setup-hook 'on-after-init))


  (global-visual-line-mode)

  ;; smartparens config
  (use-package smartparens
    :config
    (sp-local-pair '(hy-mode lisp-mode
                             emacs-lisp-mode scheme-mode
                             racket-mode clojure-mode
                             org-mode)
                   "'" "'" :actions nil))

  (defun lazy-ESC-k ()
    (interactive)
    (evil-normal-state)
    (evil-previous-line))
  (global-set-key (kbd "M-k") 'lazy-ESC-k)

  (defun my-change-window-divider ()
    (let ((display-table (or buffer-display-table standard-display-table)))
      (set-display-table-slot display-table 5 ?│)
      (set-window-display-table (selected-window) display-table)))
  (add-hook 'window-configuration-change-hook 'my-change-window-divider)

  ;; I love threading macros: let's fix the indents
  (with-eval-after-load 'dash
    (function-put '-> 'lisp-indent-function nil)
    (function-put '->> 'lisp-indent-function nil))
  (function-put 'if 'lisp-indent-function nil)

  ;; goodbye, "fd" hassle!
  (setq-default evil-escape-key-sequence nil)

  (with-eval-after-load 'pdf-view-mode
    (setq pdf-view-midnight-colors '("#B0CCDC" . "#000000")))

  (with-eval-after-load 'prolog-mode
    (setq prolog-system 'swi))

  ;; fix for opening large files in TRAMP environment
  (setq tramp-copy-size-limit 10000000
        tramp-inline-compress-start-size 10000000)

  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

  ;; I don't need these
  (spaceline-toggle-minor-modes-off)
  (spacemacs/toggle-display-time-off)
  (spacemacs/toggle-mode-line-version-control-off)

  ;; lsp config
  (use-package lsp-mode
    :hook ((tuareg-mode . lsp) (python-mode . lsp) (scala-mode . lsp))
    :config
    (when window-system
      (setq lsp-headerline-breadcrumb-enable t
            lsp-headerline-breadcrumb-icons-enable t))
    (unless window-system
      (setq lsp-headerline-breadcrumb-enable nil
            lsp-headerline-breadcrumb-icons-enable nil))
    (setq lsp-prefer-capf t
          company-idle-delay 0
          lsp-ui-sideline-enable nil
          lsp-enable-symbol-highlighting nil
          lsp-ui-doc-enable nil
          lsp-ui-doc-show-with-cursor nil
          lsp-lens-enable nil
          lsp-signature-auto-activate nil
          lsp-eldoc-hook nil
          lsp-modeline-code-actions-enable nil
          lsp-eldoc-enable-hover nil
          lsp-ui-doc-max-height 10
          lsp-enable-symbol-highlighting nil
          lsp-file-watch-threshold 5000
          lsp-metals-show-inferred-type nil
          lsp-metals-show-implicit-arguments nil
          lsp-metals-show-implicit-conversions-and-classes nil
          lsp-headerline-breadcrumb-segments '(file symbols)))

  ;; elfeed and elfeed-org config
  (with-eval-after-load 'elfeed
    (require 'elfeed-org)
    (elfeed-org)
    (setq browse-url-browser-function (lambda (url session)
                                        (if (string-match ".*youtube.com.*" url)
                                            (xwidget-webkit-browse-url url session)
                                            (w3m-browse-url url session))))
    ;; play the podcast at elfeed podcast entry
    (defun elfeed-podcast-player ()
      (interactive)
      (let ((possible-link (elfeed-entry-enclosures (elfeed-search-selected :single))))
        (emms-play-url (caar possible-link))
        (elfeed-search-untag-all-unread))))


  ;; hackernews config
  (with-eval-after-load 'hackernews
    (setq browse-url-browser-function (lambda (url session)
                                        (if (string-match ".*youtube.com.*" url)
                                            (xwidget-webkit-browse-url url session)
                                            (w3m-browse-url url session)))))

  ;; eradio config
  (with-eval-after-load 'eradio
    (spacemacs/declare-prefix "aR" "Radio")
    (spacemacs/set-leader-keys "aRp" 'eradio-play)
    (spacemacs/set-leader-keys "aRs" 'eradio-stop)
    (spacemacs/set-leader-keys "aRR" 'eradio-toggle)
    (setq eradio-player '("mpv" "--no-video" "--no-terminal" "--really-quiet")
          eradio-channels '(("MBC FM4U" . "http://serpent0.duckdns.org:8088/mbcfm.pls")
                            ("KBS CoolFM" . "http://serpent0.duckdns.org:8088/kbs2fm.pls")
                            ("SBS 파워FM" . "http://serpent0.duckdns.org:8088/sbsfm.pls")
                            ("CBS 음악방송" . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8"))))

  ;; LaTeX config
  (with-eval-after-load 'tex
    (require 'pdf-sync)
    (define-key LaTeX-mode-map (kbd "H-\\") 'TeX-previous-error)
    (define-key LaTeX-mode-map (kbd "H-/") 'TeX-next-error)
    ;; to use pdfview with auctex
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
          TeX-source-correlate-start-server t)) ;; not sure if last line is neccessary

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (setq pdf-sync-backward-display-action t
        pdf-sync-forward-display-action t)

  ;; gpg config
  (setq epg-gpg-program "gpg2")
  (unless window-system
    (setq epg-pinentry-mode 'loopback))

  ;; tron-legacy-theme config
  (setq tron-legacy-theme-softer-bg nil)

  ;; all-the-icons config
  (setq all-the-icons-scale-factor 1.0)

  ;; Projectile Config
  (setq projectile-mode-line "Projectile"
        projectile-enable-caching t
        anaconda-mode-localhost-address "localhost")

  ;; I really hate these keybindings
  (global-unset-key (kbd "H-n"))
  (global-unset-key (kbd "H-q"))
  (global-unset-key (kbd "C-x C-l"))

  ;; evil-mode config
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar)


  ;; recentf config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude "/private/var/folders/.*")
    (add-to-list 'recentf-exclude "/var/folders/.*")
    (add-to-list 'recentf-exclude "~/Mail/drafts/.*"))


  ;; eyebrowse config
  (with-eval-after-load 'eyebrowse
    (add-hook 'eyebrowse-post-window-switch-hook 'balance-windows))


  ;; flycheck config
  (with-eval-after-load 'flycheck
    (setq flycheck-checker-error-threshold 1000))


  ;; Keybinding FLEX
  ;; command-shortcuts
  (global-set-key (kbd "H-p") 'lazy-helm/helm-recentf)
  (global-set-key (kbd "H-o") 'spacemacs/helm-find-files)
  (global-set-key (kbd "H-f") 'evil-search-forward)
  (global-set-key (kbd "H-b") 'helm-buffers-list)
  (global-set-key (kbd "H-[") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "H-]") 'eyebrowse-next-window-config)
  (global-set-key (kbd "H-.") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "H-,") 'eyebrowse-next-window-config)
  (global-set-key (kbd "H-;") 'split-window-right-and-focus)
  (global-set-key (kbd "H-'") 'split-window-below-and-focus)
  (global-set-key (kbd "H-h") 'evil-window-left)
  (global-set-key (kbd "H-j") 'evil-window-down)
  (global-set-key (kbd "H-k") 'evil-window-up)
  (global-set-key (kbd "H-l") 'evil-window-right)
  (global-set-key (kbd "H-u") 'winner-undo)
  (global-set-key (kbd "H-d") 'kill-this-buffer)
  (global-set-key (kbd "H-m") 'helm-filtered-bookmarks)
  (global-set-key (kbd "H-g") 'magit)
  (global-set-key (kbd "H-r") 'winner-redo)
  (global-set-key (kbd "H-i") 'comment-dwim)
  (global-set-key (kbd "H-t") 'transpose-frame)
  (global-set-key (kbd "H-a") 'org-agenda)
  (global-set-key (kbd "H-/") 'next-error)
  (global-set-key (kbd "H-\\") 'previous-error)
  (global-set-key (kbd "H-?") 'yas-next-field)
  (global-set-key (kbd "H->") 'yas-prev-field)
  (global-set-key (kbd "H-n") 'spacemacs/new-empty-buffer-below)

  ;; command-control-shortcuts
  (global-set-key (kbd "C-H-o") (lambda ()
                                  (interactive)
                                  (insert-char ?|)))
  (global-set-key (kbd "C-H-a") (lambda ()
                                  (interactive)
                                  (insert-char ?&)))
  (global-set-key (kbd "C-H-e") 'eshell)
  (global-set-key (kbd "C-H-t") 'transpose-frame)
  (global-set-key (kbd "C-H-r") 'eradio-toggle)
  (global-set-key (kbd "C-H-f") 'spacemacs/toggle-frame-fullscreen-non-native)
  (global-set-key (kbd "C-H-g") 'helm-do-ag)
  (global-set-key (kbd "C-H-v") 'multi-vterm)
  (global-set-key (kbd "C-H-u") 'emms-pause)
  (global-set-key (kbd "C-H-[") 'emms-seek-backward)
  (global-set-key (kbd "C-H-]") 'emms-seek-forward)
  (global-set-key (kbd "C-H-p") 'previous-buffer)
  (global-set-key (kbd "C-H-n") 'next-buffer)
  (global-set-key (kbd "C-H-b") 'ibuffer)
  (global-set-key (kbd "C-H-/") 'next-error)
  (global-set-key (kbd "C-H-\\") 'previous-error)
  (global-set-key (kbd "C-H-c") 'soundklaus-tracks)
  (global-set-key (kbd "C-H-0") 'emms-volume-raise)
  (global-set-key (kbd "C-H-9") 'emms-volume-lower)
  (global-set-key (kbd "C-H-=") 'balance-windows)
  (global-set-key (kbd "C-H-w") 'markdown-preview)
  (global-set-key (kbd "C-H-4") 'play-fm4u)
  (global-set-key (kbd "C-H-i") 'imenu-list)
  (global-set-key (kbd "C-H-x") 'xwidget-new-window)
  (global-set-key (kbd "C-H-,") 'eyebrowse-close-window-config)
  (global-set-key (kbd "C-H-.") 'eyebrowse-create-window-config)
  (global-set-key (kbd "C-H-y") 'youtube-viewer-start)
  (global-set-key (kbd "C-H-;") 'flycheck-previous-error)
  (global-set-key (kbd "C-H-'") 'flycheck-next-error)
  (global-set-key [?\C-\S-h] 'evil-window-increase-width)
  (global-set-key [?\C-\S-j] 'evil-window-decrease-height)
  (global-set-key [?\C-\S-k] 'evil-window-increase-height)
  (global-set-key [?\C-\S-l] 'evil-window-decrease-width)

  ;; control-shortcuts
  (global-set-key (kbd "C-;") 'hippie-expand)

  ;; enable mouse scroll in terminal
  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
    (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)
    (setq mouse-wheel-up-event 'mouse-5
          mouse-wheel-down-event 'mouse-4))

  ;; SPC-command-shortcuts
  (spacemacs/set-leader-keys "H-r" 'revert-buffer)
  (spacemacs/set-leader-keys "C-H-f" 'spacemacs/toggle-maximize-frame-on)
  (spacemacs/set-leader-keys "H-o" 'reveal-in-osx-finder)
  (spacemacs/set-leader-keys "H-c" 'compile)
  (spacemacs/set-leader-keys "H-h" 'hackernews)
  (spacemacs/set-leader-keys "H-v" 'variable-pitch-mode)
  (spacemacs/set-leader-keys "H-f" 'spacemacs/toggle-frame-fullscreen-non-native)
  (spacemacs/set-leader-keys "H-u" 'emacs-uptime)
  (spacemacs/set-leader-keys "H-g" (lambda ()
                                     (interactive)
                                     (garbage-collect)
                                     (message "no more garbage! yay!")))
  (spacemacs/set-leader-keys "H-i" 'insert-current-time)
  (spacemacs/set-leader-keys "H-y" 'youtube-viewer-start)
  (spacemacs/set-leader-keys "." 'eyebrowse-create-window-config)
  (spacemacs/set-leader-keys "," 'eyebrowse-close-window-config)
  (spacemacs/set-leader-keys "[" 'eyebrowse-prev-window-config)
  (spacemacs/set-leader-keys "]" 'eyebrowse-next-window-config)
  (spacemacs/set-leader-keys ";" 'split-window-right-and-focus)
  (spacemacs/set-leader-keys "'" 'split-window-below-and-focus)
  (spacemacs/set-leader-keys "o" 'spacemacs/helm-find-files)
  (spacemacs/set-leader-keys "/" 'flycheck-next-error)
  (spacemacs/set-leader-keys "\\" 'flycheck-previous-error)

  ;; SPC-C-shortcuts
  (spacemacs/set-leader-keys "C-r" 'revert-buffer)
  (spacemacs/set-leader-keys "C-f" (lambda ()
                                     (interactive)
                                     (progn
                                       (lsp-format-buffer)
                                       (save-buffer))))

  ;; C-shortcuts
  (evil-define-key 'insert 'prog-mode-map (kbd "C-;") #'hippie-expand)
  (evil-define-key 'insert 'prog-mode-map (kbd "C-,") (lambda ()
                                                        (interactive)
                                                        (insert-char 40)))
  (evil-define-key 'insert 'prog-mode-map (kbd "C-.") (lambda ()
                                                        (interactive)
                                                        (insert-char 41)))

  ;; more idiosyncratic window splitting
  (spacemacs/set-leader-keys "w;" 'split-window-right-and-focus)
  (spacemacs/set-leader-keys "w'" 'split-window-below-and-focus)
  (spacemacs/set-leader-keys "w=" 'balance-windows)

  ;; magit related
  (with-eval-after-load 'magit
    (spacemacs/set-leader-keys "ga" 'magit-stage-file)
    (spacemacs/set-leader-keys "gc" 'magit-commit-create)
    (spacemacs/set-leader-keys "gp" 'magit-push)
    (spacemacs/set-leader-keys "gu" 'magit-pull))

  ;; perl config
  (with-eval-after-load 'perl-mode
    (spacemacs/set-leader-keys-for-major-mode 'perl-mode "l" 'cperl-perldoc-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'cperl-mode "l" 'cperl-perldoc-at-point)
    '(define-key perl-mode-map (kbd "C-c C-c") 'compile))

  ;; tuareg related
  (with-eval-after-load 'tuareg
    (eldoc-mode nil)
    (require 'tuareg-supplementary)
    (require 'ocamlformat)
    (setq ocamlformat-show-errors nil))

  ;; haskell-mode config
  (with-eval-after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "H-\\") 'haskell-goto-prev-error)
    (define-key haskell-mode-map (kbd "H-/") 'haskell-goto-next-error)
    (setq haskell-process-type 'stack-ghci
          haskell-interactive-popup-errors nil
          haskell-process-path-ghci "stack"))


  ;; imenu config
  (setq imenu-list-position 'left)


  (with-eval-after-load 'xwidget
    (defun xwidget-new-window ()
      (interactive)
      (let ((url (read-from-minibuffer "URL: " "https://")))
        (require 's)
        (require 'xwidget)
        (if (or (s-starts-with-p "https://https://" url)
                (s-starts-with-p "https://http://" url)
                (s-starts-with-p "http://http://" url)
                (s-starts-with-p "http://https://" url))
            (let ((trimmed (s-chop-prefixes '("https://" "http://") url)))
              (message (concat "opening " trimmed))
              (xwidget-webkit-new-session trimmed))
            (xwidget-webkit-new-session url))))
    (setq xwidget-webkit-enable-plugins t)
    (spacemacs/set-leader-keys-for-major-mode 'xwidget-webkit-mode
      "f" 'xwwp-follow-link
      "l" 'xwidget-webkit-browse-url
      "j" 'xwidget-webkit-scroll-up
      "k" 'xwidget-webkit-scroll-down
      "G" 'xwidget-webkit-scroll-bottom
      "gg" 'xwidget-webkit-scroll-top
      "u" 'xwidget-webkit-browse-url
      "h" 'xwidget-webkit-back
      "b" 'xwidget-webkit-back
      "l" 'xwidget-webkit-forward
      "H-c" 'xwidget-webkit-copy-selection-as-kill
      "q" 'kill-this-buffer)
    (add-hook 'xwidget-webkit-mode-hook
              (lambda ()
                (local-unset-key (kbd "<backspace>"))))
    (defun xwidget-webkit-find-file (file)
      (interactive "fFilename: ")
      (xwidget-webkit-new-session (w3m-expand-file-name-as-url file)))
    (spacemacs/set-leader-keys "awF" 'xwidget-webkit-find-file))


  ;; hl-todo config
  (with-eval-after-load 'hl-todo
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


  ;; scala-mode config
  (with-eval-after-load 'scala-mode
    (require 'sbt-console)
    (setq ammonite-term-repl-program-args '("--thin"))
    (spacemacs/declare-prefix-for-mode 'scala-mode "ms" "repl")
    (spacemacs/declare-prefix-for-mode 'scala-mode "me" "repl")
    (spacemacs/set-leader-keys-for-major-mode 'scala-mode
      "si" 'sbt-startup-console
      "sb" 'sbt-console-send-buffer
      "sf" 'sbt-console-send-defun
      "sr" 'sbt-console-send-region
      "sF" 'sbt-console-load-file
      "sI" 'sbt-console-send-import
      "sc" 'sbt-console-send-current-class
      "sa" 'sbt-console-send-all-classes
      "sl" 'sbt-console-convert-console-line
      "sm" 'sbt-console-send-main-contents

      "eb" 'sbt-console-send-buffer
      "ef" 'sbt-console-send-defun
      "er" 'sbt-console-send-region
      "eF" 'sbt-console-load-file
      "eI" 'sbt-console-send-import
      "ec" 'sbt-console-send-current-class
      "ea" 'sbt-console-send-all-classes
      "el" 'sbt-console-convert-console-line
      "em" 'sbt-console-send-main-contents))

  ;; emacs-lisp config
  (setq common-lisp-indent-function 'common-lisp-indent-function)

  ;; w3m config
  (with-eval-after-load 'w3m
    (setq w3m-default-display-inline-images t
          w3m-session-load-crashed-sessions 'never)
    (defun xwidget-webkit-open-w3m-current-url ()
      (require 'xwidget)
      (interactive)
      (xwidget-webkit-new-session w3m-current-url))
    (defun eww-open-w3m-current-url ()
      (interactive)
      (eww-browse-url w3m-current-url))
    (spacemacs/set-leader-keys "awx" 'xwidget-webkit-open-w3m-current-url)
    (spacemacs/set-leader-keys "awW" 'eww-open-w3m-current-url)
    (setq w3m-search-word-at-point nil
          browse-url-browser-function (lambda (url session)
                                        (if (string-match ".*youtube.com.*" url)
                                            (xwidget-webkit-browse-url url session)
                                            (w3m-browse-url url session))))
    (defun w3m-copy-current-url ()
      (interactive)
      (kill-new w3m-current-url)
      (message "Copied current URL."))
    (define-key w3m-mode-map (kbd "wc") 'w3m-copy-current-url))

  ;; Tetris config
  (with-eval-after-load 'tetris
    (define-key tetris-mode-map (kbd "C-b") 'tetris-move-left)
    (define-key tetris-mode-map (kbd "C-n") 'tetris-move-bottom)
    (define-key tetris-mode-map (kbd "C-p") 'tetris-rotate-prev)
    (define-key tetris-mode-map (kbd "C-f") 'tetris-move-right)

    (define-key tetris-mode-map (kbd "h") 'tetris-move-left)
    (define-key tetris-mode-map (kbd "j") 'tetris-move-bottom)
    (define-key tetris-mode-map (kbd "k") 'tetris-rotate-prev)
    (define-key tetris-mode-map (kbd "l") 'tetris-move-right))

  (spacemacs/declare-prefix "am" "emms")
  (spacemacs/set-leader-keys "amo" 'emms)
  (spacemacs/set-leader-keys "ams" 'emms-pause)
  (spacemacs/set-leader-keys "amp" 'emms-previous)
  (spacemacs/set-leader-keys "amn" 'emms-next)
  (spacemacs/set-leader-keys "amd" 'emms-play-directory)
  (spacemacs/set-leader-keys "amf" 'emms-play-file)
  (spacemacs/set-leader-keys "amu" 'emms-play-url)

  ;; emms config
  (with-eval-after-load 'emms
    (defun emms-mode-line-only-filename ()
      "Format the currently playing song."
      (let* ((fullname (emms-track-description
                        (emms-playlist-current-selected-track)))
             (splitted (s-split "/" fullname))
             (filename (car (last splitted))))
        (concat "🎵 " (car (s-split "\\.[mp3|wma|m4a]" filename)))))
    (require 'emms-setup)
    (emms-all)
    (emms-default-players)
    (setq emms-source-file-default-directory "~/Music/"
          emms-playlist-buffer-name "*Music*"
          emms-info-asynchronously t)
    (require 'emms-info-libtag)
    (setq emms-info-functions '(emms-info-libtag))
    (require 'emms-mode-line)
    (emms-mode-line-enable)
    (emms-mode-line 1)
    (setq emms-mode-line-mode-line-function #'emms-mode-line-only-filename)
    (require 'emms-playing-time)
    (emms-playing-time nil))

  ;; fix for run-python
  (setq python-shell-interpreter (if (string-match "aarch64.*" system-configuration)
                                     "/opt/homebrew/opt/python@3.9/libexec/bin/python"
                                     "/usr/local/opt/python@3.8/libexec/bin/python")
        python-shell-interpreter-args "-m IPython --simple-prompt -i"
        python-shell-completion-native-enable nil)

  (defun display-current-time ()
    "display the current time in the buffer."
    (interactive)
    (message (format-time-string "%Y-%m-%d %H:%M:%S")))

  (defun insert-current-time ()
    "insert the curren time at the cursor position."
    (interactive)
    (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

  ;; disable company-mode in favor of auto-complete-mode,
  ;; as the former slows down REPL in TRAMP environment.
  (defun my-python-hook ()
    (company-mode -1)
    (auto-complete-mode -1))
  (add-hook 'my-python-hook 'inferior-python-mode)

  ;; Fix for tramp
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

  ;; Common Lisp Hyperspec Offline Lookup
  ;; (load "/Users/jslee/quicklisp/clhs-use-local.el" t)

  (setq-default spacemacs-show-trailing-whitespace nil)

  ;; changing some evil default commands
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit)
  (evil-ex-define-cmd "W" 'save-buffer)
  (evil-ex-define-cmd "Q" 'kill-this-buffer)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)

  (spacemacs/toggle-mode-line-battery-off)
  (spacemacs/toggle-highlight-current-line-globally-off)

  (require 'transpose-frame)

  (setq grep-command "grep -R ")

  (spacemacs/set-leader-keys "aww" 'eww)
  (spacemacs/set-leader-keys "awm" 'w3m)
  (spacemacs/set-leader-keys "wt" 'transpose-frame)
  (spacemacs/set-leader-keys "si" 'helm-imenu)
  (spacemacs/set-leader-keys "it" 'org-insert-current-time)
  (spacemacs/set-leader-keys "ai" 'display-current-time)
  (spacemacs/set-leader-keys "ab" 'battery)
  (spacemacs/set-leader-keys "p/" 'projectile-ripgrep)

  ;; fix for dired on OSX
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash")

  ;;; org-mode config
  (with-eval-after-load 'org
    (defun org-insert-current-time ()
      "insert the curren time at the cursor position."
      (interactive)
      (insert (format-time-string "** %Y-%m-%d %H:%M:%S")))

    (setq org-startup-with-inline-images t
          org-startup-folded 'show-all
          org-startup-latex-with-latex-preview t
          org-src-fontify-natively t
          org-hide-emphasis-markers t
          org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
          org-bullets-bullet-list '("■" "◆" "▲" "▶"))

    ;; org-babel config
    (require 'ob-lisp)
    (require 'ob-clojure)
    (require 'ob-hy)
    (require 'ob-dot)

    (org-babel-do-load-languages
     'org-babel-load-languages '((lisp . t) (clojure . t)
                                 (python . t) (hy . t)
                                 (dot . t)))

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "bR" 'org-babel-remove-result)

    (setq org-todo-keywords
          '((sequence "TODO" "WORKING" "|" "DONE" "ABORTED")))

    ;; Retina Org LaTeX Preview
    (setq org-latex-create-formula-image-program 'dvisvgm)
    (eval-after-load 'org
      '(define-key org-mode-map (kbd "H-f") 'org-latex-preview))
    )

  ;; split on right & below.
  (setq evil-vsplit-window-right t
        evil-split-window-below t)

  (delete-selection-mode 1)

  ;; Korean languages settings
  (set-language-environment "Korean")
  (prefer-coding-system 'utf-8)
  (global-set-key (kbd "<f6>") 'toggle-korean-input-method)

  (with-eval-after-load 'markdown-mode
    (setq markdown-command (if (string-match "aarch64.*" system-configuration)
                               "/opt/homebrew/bin/pandoc"
                               "/usr/local/bin/pandoc")))

  (global-undo-tree-mode)

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

  (unbind-key (kbd "C-d C-l"))
  (global-set-key (kbd "C-d C-l") 'evil-toggle-input-method)
  (global-set-key (kbd "C-\\") 'evil-toggle-input-method)

  ;; graphviz config
  (with-eval-after-load 'graphviz
    (setq graphviz-dot-preview-extension "svg"))
  ) ;; user-config end

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-emoji-insert-unicode nil t)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(company-transformers
   (quote
    (company-sort-by-occurrence)))
 '(custom-safe-themes
   (quote
    ("efefb69e7781fcfe62f3d0b573701f56e45e29afbe9e378a422025fd767ea246" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#dedede" t)
 '(grep-use-null-device nil)
 '(helm-always-two-windows t)
 '(helm-bookmark-show-location t t)
 '(helm-descbinds-mode t)
 '(helm-descbinds-window-style (quote split) t)
 '(helm-display-function (quote spacemacs//display-helm-window))
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-flx-for-helm-find-files nil)
 '(helm-flx-mode t)
 '(helm-fuzzy-matching-highlight-fn (quote helm-flx-fuzzy-highlight-match))
 '(helm-fuzzy-sort-fn (quote helm-flx-fuzzy-matching-sort))
 '(helm-locate-command "mdfind -name %s %s")
 '(helm-mode t)
 '(helm-split-window-inside-p t)
 '(line-spacing 1)
 '(package-selected-packages
   (quote
    (xcscope vimrc-mode slime-company slime geiser fsharp-mode eglot xref flymake jsonrpc eldoc project anaphora dactyl-mode company-jedi jedi-core python-environment epc ctable concurrent common-lisp-snippets centaur-tabs ac-ispell \(tron-legacy\ :location\ local\)-theme ein polymode csv-mode minibuffer-line engine-mode graphviz-dot-mode transpose-frame pretty-mode tron-legacy-theme underwater-theme parseclj lua-mode company-emacs-eclim eclim w3m tidal writeroom-mode visual-fill-column poet-theme web-beautify livid-mode skewer-mode js2-refactor js2-mode js-doc company-tern tern coffee-mode clojure-snippets clj-refactor cider-eval-sexp-fu cider clojure-mode queue inflections edn multiple-cursors peg sesman parseedn a web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter evil-magit magit transient git-commit with-editor diff-hl elfeed-protocol elfeed-web elfeed-org elfeed-goodies elfeed nyan-mode ecb tronesque-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme doom-themes racket-mode faceup ensime sbt-mode scala-mode disaster company-c-headers cmake-mode clang-format company-auctex auctex-lua auctex auctex-latexmk white-sand-theme zen-and-art-theme zenburn-theme color-identifiers-mode rainbow-identifiers rainbow-mode terraform-mode hcl-mode ranger pandoc-mode ox-pandoc docker json-mode magit-popup json-snatcher json-reformat docker-tramp dockerfile-mode flycheck-ycmd ycmd request-deferred deferred esh-help eshell-prompt-extras eshell-z multi-term shell-pop xterm-color vagrant vagrant-tramp systemd spray ansible ansible-doc company-ansible jinja2-mode salt-mode mmm-jinja2 yaml-mode imenu-list nginx-mode command-log-mode rebox2 edit-server gmail-message-mode ham-mode html-to-markdown flymd osx-location rase sunshine theme-changer dash-at-point helm-dash counsel-dash dash-docs prodigy flycheck-ledger ledger-mode company-restclient know-your-http-well ob-http ob-restclient restclient-helm restclient puppet-mode fasd deft pyim pyim-basedict chinese-wbim fcitx find-by-pinyin-dired ace-pinyin pinyinlib pangu-spacing youdao-dictionary names chinese-word-at-point company-nixos-options helm-nixos-options nix-mode nixos-options selectric-mode 2048-game pacmacs sudoku typit mmt rcirc-color rcirc-notify jabber srv fsm emojify circe oauth2 websocket ht erc-terminal-notifier erc-gitter erc-hl-nicks erc-image erc-social-graph erc-view-log erc-yt nlinum-relative nlinum counsel-projectile counsel ivy-hydra smex swiper wgrep auto-dictionary flyspell-correct-ivy ivy flyspell-correct-helm flyspell-correct-popup flyspell-correct flyspell-popup floobits mwim unfill ox-twbs ox-gfm ox-reveal ibuffer-projectile bracketed-paste origami hl-anything evil-snipe evil-commentary evil-cleverparens paredit pdf-tools emms yapfify pyvenv pytest pyenv-mode py-isort pip-requirements tablist live-py-mode hy-mode dash-functional helm-pydoc emoji-cheat-sheet-plus cython-mode company-emoji company-anaconda anaconda-mode pythonic simple-httpd ace-jump-mode noflet tron-theme tron-theme-theme company-coq company-math math-symbol-lists reveal-in-osx-finder pbcopy osx-trash osx-dictionary org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download launchctl htmlize gnuplot utop tuareg caml ocp-indent merlin intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode mmm-mode markdown-toc markdown-mode helm-company helm-c-yasnippet gh-md fuzzy flycheck-pos-tip pos-tip flycheck company-statistics company auto-yasnippet yasnippet auto-complete org-plus-contrib ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(spaceline-helm-mode t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-emoji-insert-unicode nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(company-transformers '(company-sort-by-occurrence))
 '(custom-safe-themes
   '("efefb69e7781fcfe62f3d0b573701f56e45e29afbe9e378a422025fd767ea246" default))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#dedede" t)
 '(grep-use-null-device nil)
 '(helm-always-two-windows t)
 '(helm-bookmark-show-location t)
 '(helm-descbinds-mode t)
 '(helm-descbinds-window-style 'split)
 '(helm-display-function 'spacemacs//display-helm-window)
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-flx-for-helm-find-files nil)
 '(helm-flx-mode t)
 '(helm-fuzzy-matching-highlight-fn 'helm-flx-fuzzy-highlight-match)
 '(helm-fuzzy-sort-fn 'helm-flx-fuzzy-matching-sort)
 '(helm-locate-command "mdfind -name %s %s")
 '(helm-mode t)
 '(helm-split-window-inside-p t)
 '(line-spacing 1)
 '(package-selected-packages
   '(hackernews xcscope vimrc-mode slime-company slime geiser fsharp-mode eglot xref flymake jsonrpc eldoc project anaphora dactyl-mode company-jedi jedi-core python-environment epc ctable concurrent common-lisp-snippets centaur-tabs ac-ispell \(tron-legacy\ :location\ local\)-theme ein polymode csv-mode minibuffer-line engine-mode graphviz-dot-mode transpose-frame pretty-mode tron-legacy-theme underwater-theme parseclj lua-mode company-emacs-eclim eclim w3m tidal writeroom-mode visual-fill-column poet-theme web-beautify livid-mode skewer-mode js2-refactor js2-mode js-doc company-tern tern coffee-mode clojure-snippets clj-refactor cider-eval-sexp-fu cider clojure-mode queue inflections edn multiple-cursors peg sesman parseedn a web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter evil-magit magit transient git-commit with-editor diff-hl elfeed-protocol elfeed-web elfeed-org elfeed-goodies elfeed nyan-mode ecb tronesque-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme doom-themes racket-mode faceup ensime sbt-mode scala-mode disaster company-c-headers cmake-mode clang-format company-auctex auctex-lua auctex auctex-latexmk white-sand-theme zen-and-art-theme zenburn-theme color-identifiers-mode rainbow-identifiers rainbow-mode mu4e-alert mu4e-maildirs-extension terraform-mode hcl-mode ranger pandoc-mode ox-pandoc docker json-mode magit-popup json-snatcher json-reformat docker-tramp dockerfile-mode flycheck-ycmd ycmd request-deferred deferred esh-help eshell-prompt-extras eshell-z multi-term shell-pop xterm-color vagrant vagrant-tramp systemd spray ansible ansible-doc company-ansible jinja2-mode salt-mode mmm-jinja2 yaml-mode imenu-list nginx-mode command-log-mode rebox2 edit-server gmail-message-mode ham-mode html-to-markdown flymd osx-location rase sunshine theme-changer dash-at-point helm-dash counsel-dash dash-docs prodigy flycheck-ledger ledger-mode company-restclient know-your-http-well ob-http ob-restclient restclient-helm restclient puppet-mode fasd deft pyim pyim-basedict chinese-wbim fcitx find-by-pinyin-dired ace-pinyin pinyinlib pangu-spacing youdao-dictionary names chinese-word-at-point company-nixos-options helm-nixos-options nix-mode nixos-options selectric-mode 2048-game pacmacs sudoku typit mmt rcirc-color rcirc-notify jabber srv fsm emojify circe oauth2 websocket ht erc-terminal-notifier erc-gitter erc-hl-nicks erc-image erc-social-graph erc-view-log erc-yt nlinum-relative nlinum counsel-projectile counsel ivy-hydra smex swiper wgrep auto-dictionary flyspell-correct-ivy ivy flyspell-correct-helm flyspell-correct-popup flyspell-correct flyspell-popup floobits mwim unfill ox-twbs ox-gfm ox-reveal ibuffer-projectile bracketed-paste origami hl-anything evil-snipe evil-commentary evil-cleverparens paredit pdf-tools emms yapfify pyvenv pytest pyenv-mode py-isort pip-requirements tablist live-py-mode hy-mode dash-functional helm-pydoc emoji-cheat-sheet-plus cython-mode company-emoji company-anaconda anaconda-mode pythonic simple-httpd ace-jump-mode noflet tron-theme tron-theme-theme company-coq company-math math-symbol-lists reveal-in-osx-finder pbcopy osx-trash osx-dictionary org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download launchctl htmlize gnuplot utop tuareg caml ocp-indent merlin intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode mmm-mode markdown-toc markdown-mode helm-company helm-c-yasnippet gh-md fuzzy flycheck-pos-tip pos-tip flycheck company-statistics company auto-yasnippet yasnippet auto-complete org-plus-contrib ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(spaceline-helm-mode t)
 '(tab-stop-list
   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
