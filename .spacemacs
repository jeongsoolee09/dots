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
   '(;; ============ Spacemacs functionalities ============
     syntax-checking
     spacemacs-modeline
     spacemacs-layouts
     (auto-completion :variables company-mode-completion-cancel-keywords nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior nil
                      cargo-process-reload-on-modify t)
     (elfeed :variables rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
     (mu4e :variables mu4e-installation-path (if (string-match "aarch64.*" system-configuration)
                                                 "/opt/homebrew/Cellar/mu/1.8.8/share/emacs/site-lisp/mu/mu4e"
                                                 "/usr/local/Cellar/mu/1.8.8/share/emacs/site-lisp/mu/mu4e"))
     (lsp :variables
          auto-completion-idle-delay 0
          lsp-rust-server 'rust-analyzer)
     (colors :variables colors-colorize-identifiers 'all)
     (copy-as-format :variables copy-as-format-default "markdown"
                     copy-as-format-asciidoc-include-file-name t
                     copy-as-format-asciidoc-language-alist '(("^.*\\.java$" "java")
                                                              ("^.*\\.clj[cs]?$" "clojure")))
     (compleseus :variables compleseus-engine 'vertico)
     helm
     git
     version-control
     osx
     pdf
     ibuffer
     epub
     w3m
     hackernews
     xclipboard
     restclient
     (rcirc :variables rcirc-enable-authinfo-support t)
     (erc :variables
          erc-enable-sasl-auth t
          erc-enable-notifications nil)
     docker
     kubernetes
     bm
     dotnet
     djvu
     finance
     helpful
     imenu-list
     ietf
     pandoc
     multiple-cursors
     (streamlink :variables streamlink-player "mpv --no-video")
     emoji
     pocket
     reddit
     games
     theming
     evil-better-jumper
     evil-snipe
     command-log
     spotify
     myleetcode
     ;; ============ languages ============
     (tree-sitter :variables
                  spacemacs-tree-sitter-hl-black-list '(js2-mode rjsx-mode)
                  tree-sitter-syntax-highlight-enable t
                  tree-sitter-fold-enable t
                  tree-sitter-fold-indicators-enable nil)
     major-modes
     graphql
     (javascript :variables javascript-backend 'lsp
                 js2-basic-offset 2)
     (typescript :variables typescript-backend 'lsp)
     (solidity :variables solidity-flycheck-solium-checker-active t)
     (clojure :variables clojure-backend 'lsp)
     (html :variables css-enable-lsp t
           less-enable-lsp t
           scss-enable-lsp t
           html-enable-lsp t)
     (c-c++ :variables c-c++-backend 'lsp-clangd)
     (org :variables
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-roam-support t
          org-enable-roam-protocol t
          org-enable-roam-ui t
          org-roam-complete-everywhere t
          org-enable-org-brain-support t
          org-enable-org-journal-support t
          org-journal-dir "~/Dropbox/Org-Journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-enable-reveal-js-support t
          org-superstar-bullet-list '("■" "◆" "▲" "▶")
          org-enable-valign t
          org-enable-asciidoc-support t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'vterm)
     (haskell :variables haskell-backend 'lsp)
     (purescript :variables purescript-backend 'lsp)
     (elm :variables elm-backend 'lsp)
     idris
     agda
     (ocaml :variables ocaml-backend 'lsp)
     (markdown :variables markdown-live-preview-engine 'vmd)
     asciidoc
     restructuredtext
     (python :variables python-backend 'lsp
             python-lsp-server 'mspyls)
     (csharp :variables csharp-backend 'lsp)
     (fsharp :variables fsharp-backend 'lsp)
     (java :variables java-backend 'lsp)
     coq
     racket
     emacs-lisp
     graphviz
     csv
     ;; ipython-notebook
     common-lisp
     hy
     (scheme :variables
             scheme-implementations '(chez chicken gambit guile kawa racket)
             scheme-program-name "guile")
     prolog
     django
     (nim :variables nim-backend 'lsp)
     (yaml :variables yaml-enable-lsp t)
     (latex :variables latex-build-command "LatexMk")
     (dart :variables dart-backend 'lsp)
     (scala :variables scala-backend 'lsp)
     (ruby :variables ruby-backend 'lsp)
     (perl5 :variables perl5-backend 'lsp)
     (plantuml :variables plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2022.5/libexec/plantUml.jar"
               org-plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2022.5/libexec/plantUml.jar")
     mermaid
     (vimscript :variables vimscript-backend 'lsp)
     (rust :variables rust-backend 'lsp)
     (go :variables go-backend 'lsp)
     (kotlin :variables kotlin-backend 'lsp)
     (lua :variables lua-backend 'lsp
          lua-lsp-server 'lua-language-server)
     (sql :variables sql-backend 'lsp
          sql-lsp-sqls-workspace-config-path 'workspace)
     (crystal :variables crystal-backend 'lsp)
     (zig :variables zls-backend 'lsp)
     (ess :variables ess-r-backend 'lsp)
     (julia :variables julia-backend 'lsp)
     (react :variables react-backend 'lsp)
     (vue :variables vue-backend 'lsp)
     (svelte :variables svelte-backend 'lsp)
     (elixir :variables elixir-backend 'lsp)
     (erlang :variables erlang-backend 'lsp)
     tidalcycles)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(transpose-frame emms sicp s ts multi-vterm eradio
                                                      xwwp ag emamux format-all mermaid-mode
                                                      rg python-pytest lsp-docker lsp-rescript
                                                      web-server org-journal org-kanban org-link-beautify
                                                      org-listcruncher org-noter org-roam-bibtex citar
                                                      grip-mode fennel-mode fstar-mode eshell-did-you-mean
                                                      eshell-git-prompt eshell-info-banner eshell-syntax-highlighting
                                                      deadgrep tron-legacy-theme trie
                                                      clomacs clj-decompiler clj-deps-new clj-refactor inf-clojure
                                                      inf-elixir parseclj helm-cider-history janet-mode ob-rust
                                                      ob-kotlin mixed-pitch a org-auto-tangle ob-async
                                                      tao-theme geiser-chicken geiser-guile geiser-mit
                                                      coterm plz twitch-api
                                                      (kbd-mode
                                                       :location
                                                       (recipe
                                                        :fetcher github
                                                        :repo "kmonad/kbd-mode")))
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
   dotspacemacs-install-packages 'used-but-keep-unused))

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
   ;; dotspacemacs-startup-banner (concat spacemacs-banner-directory "img/eve-transparent.png")
   ;; dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '()
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(modus-operandi modus-vivendi)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 20.0
                               :weight normal
                               :width normal)
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
   dotspacemacs-distinguish-gui-tab t
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
   dotspacemacs-fullscreen-at-startup t
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
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator slant :separator-scale 1.3)
   ;; dotspacemacs-mode-line-theme '(all-the-icons :separator contour)
   dotspacemacs-mode-line-theme '(doom)
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
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
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq-default quelpa-build-tar-executable (executable-find "gtar"))

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))


  (setq explicit-shell-file-name "/bin/zsh"
        shell-file-name "/bin/zsh")


  ;; Fix for dired in TRAMP environment
  (add-hook 'dired-mode-hook
            (lambda ()
              (when (file-remote-p dired-directory)
                (setq-local dired-actual-switches "-alhB"))))


  ;; only for emacs-mac
  (mac-auto-operator-composition-mode)


  (setq dotspacemacs-scroll-bar-while-scrolling nil)


  (setq warning-minimum-level :emergency
        warning-minimum-log-level :emergency
        native-comp-async-report-warnings-errors nil)


  (setq org-roam-directory "~/Dropbox/Roam")
  (setq theming-modifications
        '((tron-legacy
           (erc-input-face :foreground "#C0FFEE")
           (mode-line :background "#000000")
           (mode-line-inactive :background "#000000")
           (tool-bar :background "#000000")
           (lsp-face-highlight-textual :weight bold :underline t))
          (tao-yang
           (lsp-face-highlight-textual :weight bold :underline t))
          (modus-vivendi
           (erc-input-face :foreground "#C0FFEE")
           (tool-bar :background "#000000")
           (lsp-face-highlight-textual :weight bold :underline t))
          (modus-operandi
           (tool-bar :background "#d7d7d7")
           (font-lock-keyword-face :foreground "#5317ac" :weight bold))
          (wheatgrass
           (cursor :background "wheat"))))


  (unless (display-graphic-p)
    ;; activate mouse-based scrolling
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))


  ;; disable spacemacs buffer
  (defun spacemacs-buffer/goto-buffer (&optional refresh))
  (defun spacemacs-buffer/display-startup-note ())
  (defun spacemacs-buffer//startup-hook ())
  (setq initial-buffer-choice t)
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

  ;; dired config
  (spacemacs/set-leader-keys "fF" 'find-name-dired)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash")

  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

  ;; doom-modeline config
  (setq-default doom-modeline-height 10)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (spacemacs/set-leader-keys "Tl" 'hidden-mode-line-mode)


  (spacemacs|do-after-display-system-init
   ;; For emacs-mac, avoid the notch
   (spacemacs/load-spacemacs-env)
   (setq dotspacemacs-scroll-bar-while-scrolling nil))


  ;; transparent emacs in terminal
  (unless window-system
    (defun on-after-init ()
      (unless (display-graphic-p (selected-frame))
        (set-face-background 'default "unspecified-bg" (selected-frame))))
    (add-hook 'window-setup-hook 'on-after-init))


  (global-visual-line-mode)
  (setq browse-url-browser-function (lambda (url session)
                                      (if (or (string-match ".*youtube.com.*" url)
                                              (string-match ".*youtu.be.*" url))
                                          (xwidget-webkit-browse-url url session)
                                          (eww-browse-url url))))


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
    (function-put '->> 'lisp-indent-function nil)
    (function-put 'if 'lisp-indent-function nil))


  ;; goodbye, "fd" hassle!
  (setq-default evil-escape-key-sequence nil)


  ;; pdf-view-mode config
  (setq pdf-view-midnight-colors '("#B0CCDC" . "#000000"))


  ;; prolog mode config
  (with-eval-after-load 'prolog-mode
    (setq prolog-system 'swi))


  ;; TRAMP config
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    ;; fix for opening large files in TRAMP environment
    (setq tramp-copy-size-limit 10000000
          tramp-inline-compress-start-size 10000000))


  ;; lsp-mode config
  (use-package lsp-mode
    :hook '((tuareg-mode . lsp))
    :config
    (when window-system
      (setq lsp-headerline-breadcrumb-enable t)
      (setq lsp-headerline-breadcrumb-icons-enable t))
    (unless window-system
      (setq lsp-headerline-breadcrumb-enable nil)
      (setq lsp-headerline-breadcrumb-icons-enable nil))
    (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/jdk-11.0.8.jdk/Contents/Home/bin/java"
          lsp-prefer-capf t
          company-idle-delay 0
          company-tooltip-idle-delay 0
          company-echo-delay 0
          lsp-idle-delay 0
          lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil
          lsp-ui-doc-show-with-cursor nil
          lsp-lens-enable nil
          lsp-ui-doc-max-height 10
          lsp-diagnostics-mode nil
          lsp-diagnostics-mode-hook nil
          lsp-eldoc-hook nil
          lsp-eldoc-enable-hover nil
          lsp-signature-auto-activate t
          lsp-metals-show-inferred-type nil
          lsp-metals-show-implicit-arguments nil
          lsp-metals-show-implicit-conversions-and-classes nil
          lsp-headerline-breadcrumb-segments '(file symbols)))


  ;; rescript-mode config
  (with-eval-after-load 'rescript-mode
    ;; Tell `lsp-mode` about the `rescript-vscode` LSP server
    (setq lsp-rescript-server-command '("node" "/Users/jslee/.emacs.d/rescript-server/server/out/server.js" "--stdio"))
    (require 'lsp-rescript)
    ;; Enable `lsp-mode` in rescript-mode buffers
    (add-hook 'rescript-mode-hook 'lsp-deferred)
    ;; Enable display of type information in rescript-mode buffers
    (require 'lsp-ui)
    (add-hook 'rescript-mode-hook 'lsp-ui-doc-mode))


  ;; elfeed config
  (with-eval-after-load 'elfeed
    (require 'elfeed-org)
    (elfeed-org)
    ;; play the podcast at elfeed podcast entry
    (defun elfeed-player ()
      (interactive)
      (let ((enclosure-link (elfeed-entry-enclosures (elfeed-search-selected :single)))
            (entry-link (elfeed-entry-link (elfeed-search-selected :single))))
        (if enclosure-link
            (emms-play-url (caar enclosure-link))
            (emms-play-url entry-link))
        (elfeed-search-untag-all-unread)))
    (defun elfeed-youtube-player ()
      (interactive)
      (let ((entry-link (elfeed-entry-link (elfeed-search-selected :single))))
        (async-shell-command (concat "mpv " "'" entry-link "'") nil nil)
        (elfeed-search-untag-all-unread)))
    (define-key elfeed-search-mode-map (kbd "P") #'elfeed-player)
    (define-key elfeed-search-mode-map (kbd "Y") #'elfeed-youtube-player))


  ;; hy config
  (spacemacs/set-leader-keys-for-major-mode 'hy-mode "ef" 'hy-shell-eval-current-form)
  (spacemacs/set-leader-keys-for-major-mode 'hy-mode "ee" 'hy-shell-eval-last-sexp)
  (add-hook 'hy-mode-hook #'spacemacs/toggle-auto-completion-off)
  (add-hook 'inferior-hy-mode-hook #'spacemacs/toggle-auto-completion-off)


  ;; clojure config
  (defun run-bb ()
    (interactive)
    (comint-run "bb" '()))
  (defun run-nbb ()
    (interactive)
    (comint-run "nbb" '()))
  (spacemacs/set-leader-keys "atsb" 'run-bb)
  (spacemacs/set-leader-keys "atsn" 'run-nbb)
  (with-eval-after-load 'cider
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
      (context 2)))


  ;; kotlin config
  (with-eval-after-load 'kotlin-mode
    (defun run-kotlin ()
      (interactive)
      (comint-run "kotlin" '()))
    (spacemacs/set-leader-keys-for-major-mode 'kotlin-mode "'" #'run-kotlin))


  ;; reddigg config
  (setq reddigg-subs '(emacs clojure orgmode lisp commandline
                             mechkeyboard scala haskell HHKB clojure
                             vim kotlin programmerhumor orgmode
                             commandline CityPorn OrgRoam))


  ;; eradio config
  (spacemacs/declare-prefix "aR" "Radio")
  (spacemacs/set-leader-keys "aRp" 'eradio-play)
  (spacemacs/set-leader-keys "aRs" 'eradio-stop)
  (spacemacs/set-leader-keys "aRR" 'eradio-toggle)
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
                          ("CBS 음악방송" . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8")))


  ;; LaTeX config
  (with-eval-after-load 'tex
    (require 'pdf-sync)
    ;; to use pdfview with auctex
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
          TeX-source-correlate-start-server t)) ;; not sure if last line is neccessary


  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq pdf-sync-backward-display-action t
        pdf-sync-forward-display-action t)


  ;; gpg config
  (setq epg-gpg-program "gpg")
  (unless window-system
    (setq epg-pinentry-mode 'loopback))


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


  ;; eshell config
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda ()
                                  (company-mode -1))))

  ;; flycheck config
  (with-eval-after-load 'flycheck
    (setq flycheck-checker-error-threshold 1000))


  ;; leetcode config
  (setq leetcode-prefer-language "racket")
  (setq leetcode-prefer-sql "mysql")


  (defun kill-helm-buffers ()
    (interactive)
    (dolist (buffer (remove-if-not (lambda (buffer)
                                     (s-starts-with? "*helm"
                                                     (buffer-name buffer)))
                                   (buffer-list)))
      (kill-buffer buffer)))


  (defun cleanup-emacs ()
    (interactive)
    (garbage-collect)
    (when (featurep 'helpful)
      (helpful-kill-buffers))
    (recentf-cleanup)
    (kill-helm-buffers)
    (message "no more garbage! yay!"))


  ;; Keybinding FLEX
  ;; command-shortcuts
  (global-set-key (kbd "H-p") 'lazy-helm/helm-recentf)
  (global-set-key (kbd "H-o") 'spacemacs/helm-find-files)
  (global-set-key (kbd "H-f") 'spacemacs/toggle-frame-fullscreen-non-native)
  (global-set-key (kbd "H-b") 'helm-buffers-list)
  (global-set-key (kbd "H-e") 'eshell)
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
  (global-set-key (kbd "H-g") 'magit)
  (global-set-key (kbd "H-r") 'winner-redo)
  (global-set-key (kbd "H-t") 'spacemacs/toggle-tool-bar)
  (global-set-key (kbd "H-a") 'org-agenda)
  (global-set-key (kbd "H-c") 'org-capture)
  (global-set-key (kbd "H-/") 'eyebrowse-create-window-config)
  (global-set-key (kbd "H-\\") 'eyebrowse-close-window-config)
  (global-set-key (kbd "H-?") 'yas-next-field)
  (global-set-key (kbd "H->") 'yas-prev-field)
  (global-set-key (kbd "H-<return>") 'helm-spotify-plus)

  ;; command-control-shortcuts
  (global-set-key (kbd "C-H-o") (lambda ()
                                  (interactive)
                                  (insert-char ?|)))
  (global-set-key (kbd "C-H-a") (lambda ()
                                  (interactive)
                                  (insert-char ?&)))
  (global-set-key (kbd "C-H-e") 'eww)
  (global-set-key (kbd "C-H-t") 'spacemacs/cycle-spacemacs-theme)
  (global-set-key (kbd "C-H-r") 'eradio-toggle)
  (global-set-key (kbd "C-H-f") 'spacemacs/toggle-frame-fullscreen-non-native)
  (global-set-key (kbd "C-H-g") 'helm-do-ag)
  (global-set-key (kbd "C-H-v") 'multi-vterm)
  (global-set-key (kbd "C-H-u") 'emms-pause)
  (global-set-key (kbd "H-<escape>") 'emms-seek-backward)
  (global-set-key (kbd "C-H-]") 'emms-seek-forward)
  (global-set-key (kbd "C-H-p") 'previous-buffer)
  (global-set-key (kbd "C-H-n") 'next-buffer)
  (global-set-key (kbd "C-H-b") 'ibuffer)
  (global-set-key (kbd "C-H-c") 'org-capture)
  (global-set-key (kbd "C-H-0") 'emms-volume-raise)
  (global-set-key (kbd "C-H-9") 'emms-volume-lower)
  (global-set-key (kbd "C-H-=") 'balance-windows)
  (global-set-key (kbd "C-H-4") 'play-fm4u)
  (global-set-key (kbd "C-H-i") 'imenu-list-smart-toggle)
  (global-set-key (kbd "C-H-x") 'xwidget-new-window)
  (global-set-key (kbd "C-H-,") 'previous-error)
  (global-set-key (kbd "C-H-.") 'next-error)
  (global-set-key (kbd "C-H-y") 'youtube-viewer-start)
  (global-set-key (kbd "C-H-;") 'flycheck-previous-error)
  (global-set-key (kbd "C-H-'") 'flycheck-next-error)
  (global-set-key [?\C-\S-h] 'evil-window-increase-width)
  (global-set-key [?\C-\S-j] 'evil-window-decrease-height)
  (global-set-key [?\C-\S-k] 'evil-window-increase-height)
  (global-set-key [?\C-\S-l] 'evil-window-decrease-width)

  ;; control-shortcuts
  (global-set-key (kbd "C-;") 'hippie-expand)
  (global-set-key (kbd "C-M-;") 'completion-at-point)
  (global-set-key (kbd "C-=") 'lsp-format-buffer)


  ;; SPC-command-shortcuts
  (spacemacs/set-leader-keys "H-r" 'revert-buffer)
  (spacemacs/set-leader-keys "H-t" 'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)
  (spacemacs/set-leader-keys "C-H-f" 'spacemacs/toggle-maximize-frame-on)
  (spacemacs/set-leader-keys "H-o" 'reveal-in-osx-finder)
  (spacemacs/set-leader-keys "H-c" 'compile)
  (spacemacs/set-leader-keys "H-h" 'hackernews)
  (spacemacs/set-leader-keys "H-v" 'variable-pitch-mode)
  (spacemacs/set-leader-keys "H-f" 'spacemacs/toggle-frame-fullscreen-non-native)
  (spacemacs/set-leader-keys "H-u" 'emacs-uptime)
  (spacemacs/set-leader-keys "H-g" 'cleanup-emacs)
  (spacemacs/set-leader-keys "H-i" 'insert-current-time)
  (spacemacs/set-leader-keys "H-y" 'youtube-viewer-start)
  (spacemacs/set-leader-keys "H-p" 'evil-unimpaired/next-frame)
  (spacemacs/set-leader-keys "H-n" 'evil-unimpaired/previous-frame)
  (spacemacs/set-leader-keys "." 'eyebrowse-create-window-config)
  (spacemacs/set-leader-keys "," 'eyebrowse-close-window-config)
  (spacemacs/set-leader-keys "[" 'eyebrowse-prev-window-config)
  (spacemacs/set-leader-keys "]" 'eyebrowse-next-window-config)
  (spacemacs/set-leader-keys ";" 'split-window-right-and-focus)
  (spacemacs/set-leader-keys "'" 'split-window-below-and-focus)
  (spacemacs/set-leader-keys "o" 'spacemacs/helm-find-files)
  (spacemacs/set-leader-keys "/" 'flycheck-next-error)
  (spacemacs/set-leader-keys "\\" 'flycheck-previous-error)
  (spacemacs/set-leader-keys "M-e" (lambda ()
                                     (interactive)
                                     (spacemacs/force-init-spacemacs-env)
                                     (spacemacs/load-spacemacs-env)
                                     (message ".spacemacs.env loaded.")))


  ;; SPC-C-shortcuts
  (spacemacs/set-leader-keys "C-r" 'revert-buffer)
  (spacemacs/set-leader-keys "C-f" (lambda ()
                                     (interactive)
                                     (progn
                                       (funcall-interactively #'format-all-buffer)
                                       (save-buffer))))


  ;; C-shortcuts
  (evil-define-key 'insert 'prog-mode-map (kbd "C-;") #'hippie-expand)
  (evil-define-key 'insert 'prog-mode-map (kbd "C-,") (lambda ()
                                                        (interactive)
                                                        (insert-char 40)))
  (evil-define-key 'insert 'prog-mode-map (kbd "C-.") (lambda ()
                                                        (interactive)
                                                        (insert-char 41)))
  (evil-define-key 'insert 'prog-mode-map (kbd "C-]") #'dumb-jump)
  (evil-define-key 'normal 'prog-mode-map (kbd "C-]") #'dumb-jump)

  ;; more idiosyncratic window splitting
  (spacemacs/set-leader-keys "w;" 'split-window-right-and-focus)
  (spacemacs/set-leader-keys "w'" 'split-window-below-and-focus)
  (spacemacs/set-leader-keys "w=" 'balance-windows)

  ;; magit related
  (spacemacs/set-leader-keys "ga" 'magit-stage-file)
  (spacemacs/set-leader-keys "gc" 'magit-commit-create)
  (spacemacs/set-leader-keys "gp" 'magit-push)
  (spacemacs/set-leader-keys "gu" 'magit-pull)
  (setq forge-add-default-bindings nil)


  ;; perl config
  (with-eval-after-load 'perl-mode
    (spacemacs/set-leader-keys-for-major-mode 'perl-mode "l" 'cperl-perldoc-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'cperl-mode "l" 'cperl-perldoc-at-point)
    '(define-key perl-mode-map (kbd "C-c C-c") 'compile))


  ;; lua config
  (defun run-hammerspoon ()
    (interactive)
    (comint-run "hs" '()))
  (with-eval-after-load 'lua-mode
    (spacemacs/declare-prefix-for-mode 'lua-mode "n" "hammerspoon")
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode
      "n'" 'run-hammerspoon)

    (defun toggle-lua-process-buffer ()
      "Swap between *lua* and *hs*, depending on the current lua process."
      (interactive)
      (let ((lua-process-buffer-name (buffer-name lua-process-buffer)))
        (cond ((string= lua-process-buffer-name "*lua*")
               (let ((hammerspoon-buffer (get-buffer "*hs*")))
                 (if hammerspoon-buffer
                     (progn
                       (setq lua-process (get-buffer-process hammerspoon-buffer)
                             lua-process-buffer hammerspoon-buffer)
                       (comint-send-string (get-buffer-process hammerspoon-buffer)
                                            (concat lua-process-init-code "\n")))
                     (progn
                       (run-hammerspoon)
                       (let ((new-hammerspoon-buffer (get-buffer "*hs*")))
                         (setq lua-process (get-buffer-process new-hammerspoon-buffer)
                               lua-process-buffer new-hammerspoon-buffer)
                         (comint-send-string (get-buffer-process new-hammerspoon-buffer)
                                             (concat lua-process-init-code "\n")))))
                 (message "Switched to Hammerspoon.")))
              ((string= lua-process-buffer-name "*hs*")
               (let ((lua-buffer (get-buffer "*lua*")))
                 (if lua-buffer
                     (progn
                       (setq lua-process (get-buffer-process lua-buffer)
                             lua-process-buffer lua-buffer)
                       (comint-send-string (get-buffer-process lua-buffer)
                                           (concat lua-process-init-code "\n")))
                     (progn
                       (run-lua)
                       (let ((new-lua-buffer (get-buffer "*lua*")))
                         (setq lua-process (get-buffer-process new-lua-buffer)
                               lua-process-buffer new-lua-buffer)
                         (comint-send-string (get-buffer-process new-lua-buffer)
                                             (concat lua-process-init-code "\n")))))
                 (message "Switched to Lua."))))))
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "n'" #'toggle-lua-process-buffer))


  ;; fennel config
  (with-eval-after-load 'fennel-mode
    (dolist (prefix '(("me" . "eval")
                      ("mg" . "goto")
                      ("mh" . "help")))
      (spacemacs/declare-prefix-for-mode
        'fennel-mode (car prefix) (cdr prefix)))
    (spacemacs/set-leader-keys-for-major-mode 'fennel-mode
      "'" 'fennel-repl
      "r" 'fennel-reload

      "ea" 'lisp-show-arglist
      "eb" 'eval-buffer
      "ee" 'lisp-eval-last-sexp
      "ef" 'lisp-eval-defun
      "eL" 'fennel-view-compilation
      "en" 'lisp-eval-form-and-next
      "ep" 'lisp-eval-paragraph
      "er" 'lisp-eval-region
      "hd" 'lisp-describe-sym))


  ;; tuareg related
  (with-eval-after-load 'tuareg
    (eldoc-mode nil)
    (require 'tuareg-supplementary)
    (require 'ocamlformat)
    (setq ocamlformat-show-errors nil)
    (setq utop-command "opam config exec -- dune utop . -- -emacs")
    (defun ocamlformat-newline-and-indent-new ()
      (interactive)
      (if (buffer-file-name)
          (ocamlformat-newline-and-indent)
          (insert-char 10)))
    (define-key tuareg-mode-map (kbd "RET") #'ocamlformat-newline-and-indent-new)
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "C-f" (lambda ()
                                                                   (interactive)
                                                                   (ocamlformat)
                                                                   (save-buffer))))


  ;; haskell-mode config
  (with-eval-after-load 'haskell-mode
    (setq haskell-process-type 'stack-ghci
          haskell-interactive-popup-errors nil
          haskell-process-path-ghci "stack"))

  ;; agda0mode config
  (with-eval-after-load 'agda-mode
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate"))))

  ;; imenu config
  (setq imenu-list-position 'left)


  ;; xwidget config
  (require 'xwidget)
  (with-eval-after-load 'xwidget
    (defun xwidget-new-window ()
      (interactive)
      (let ((url (read-from-minibuffer "URL: " "https://")))
        (if (or (s-starts-with-p "https://https://" url)
                (s-starts-with-p "https://http://" url)
                (s-starts-with-p "http://http://" url)
                (s-starts-with-p "http://https://" url))
            (let ((trimmed (s-chop-prefixes '("https://" "http://") url)))
              (message (concat "opening " trimmed))
              (xwidget-webkit-new-session trimmed))
            (xwidget-webkit-new-session url))))

    (defun xwidget-open-url-in-new-window (url)
      (interactive)
      (if (or (s-starts-with-p "https://https://" url)
              (s-starts-with-p "https://http://" url)
              (s-starts-with-p "http://http://" url)
              (s-starts-with-p "http://https://" url))
          (let ((trimmed (s-chop-prefixes '("https://" "http://") url)))
            (message (concat "opening " trimmed))
            (xwidget-webkit-new-session trimmed))
          (xwidget-webkit-new-session url)))

    (defun xwidget-webkit-copy-current-url ()
      (interactive)
      (let ((current-url (->> (xwidget-webkit-current-url)
                              (s-chop-prefix "URL: "))))
        (kill-new current-url)))

    (setq xwidget-webkit-enable-plugins t)
    (spacemacs/set-leader-keys-for-major-mode 'xwidget-webkit-mode
      "f" 'xwwp-follow-link
      "l" 'xwidget-webkit-browse-url
      "j" 'xwidget-webkit-scroll-up
      "k" 'xwidget-webkit-scroll-down
      "r" 'xwidget-webkit-reload
      "G" 'xwidget-webkit-scroll-bottom
      "gg" 'xwidget-webkit-scroll-top
      "u" 'xwidget-webkit-browse-url
      "h" 'xwidget-webkit-back
      "b" 'xwidget-webkit-back
      "l" 'xwidget-webkit-forward
      "y" 'xwidget-webkit-copy-selection-as-kill
      "H-c" 'xwidget-webkit-copy-selection-as-kill
      "c" 'xwidget-webkit-copy-current-url
      "q" 'kill-this-buffer)
    (add-hook 'xwidget-webkit-mode-hook
              (lambda ()
                (local-unset-key (kbd "<backspace>"))))

    (defun xwidget-webkit-find-file (file)
      (interactive "fFilename: ")
      (xwidget-webkit-new-session (w3m-expand-file-name-as-url file)))

    (spacemacs/set-leader-keys "awF" 'xwidget-webkit-find-file)

    (defun xwidget-webkit-open-localhost ()
      (interactive)
      (xwidget-webkit-new-session (concat "http://localhost:"
                                          (read-from-minibuffer "Port: " "8888")))))


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


  ;; world-clock config
  (setq world-clock-list t)
  (setq zoneinfo-style-world-list '(("America/Los_Angeles" "Los Angeles")
                                    ("America/New_York" "New York")
                                    ("Asia/Seoul" "Seoul")))
  (spacemacs/declare-prefix "aC" "clock")
  (spacemacs/set-leader-keys "aCw" #'world-clock)


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


  ;; common lisp config
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)


  ;; w3m config
  (with-eval-after-load 'w3m
    (setq w3m-default-display-inline-images t
          w3m-session-load-crashed-sessions 'never)
    (defun xwidget-webkit-open-w3m-current-url ()
      (interactive)
      (xwidget-webkit-new-session w3m-current-url))
    (defun eww-open-w3m-current-url ()
      (interactive)
      (eww-browse-url w3m-current-url))
    (spacemacs/set-leader-keys "awx" 'xwidget-webkit-open-w3m-current-url)
    (spacemacs/set-leader-keys "awW" 'eww-open-w3m-current-url)
    (setq w3m-search-word-at-point nil)
    (defun w3m-copy-current-url ()
      (interactive)
      (kill-new w3m-current-url)
      (message "Copied current URL."))
    (define-key w3m-mode-map (kbd "wc") 'w3m-copy-current-url))


  ;; eww config
  (with-eval-after-load 'eww
    (defun eww-open-w3m-current-url ()
      (interactive)
      (w3m-browse-url (eww-copy-page-url)))
    (spacemacs/set-leader-keys (kbd "awM") 'eww-open-w3m-current-url)
    (evil-define-key 'normal eww-mode-map (kbd "c") 'eww-copy-page-url)
    (setq eww-search-prefix "https://www.google.com/search?q="))


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


  (spacemacs/declare-prefix "ame" "emms")
  (spacemacs/set-leader-keys "ameo" 'emms)
  (spacemacs/set-leader-keys "ames" 'emms-pause)
  (spacemacs/set-leader-keys "amep" 'emms-previous)
  (spacemacs/set-leader-keys "amen" 'emms-next)
  (spacemacs/set-leader-keys "amed" 'emms-play-directory)
  (spacemacs/set-leader-keys "amef" 'emms-play-file)
  (spacemacs/set-leader-keys "ameu" 'emms-play-url)


  ;; emms config
  (with-eval-after-load 'emms
    (defun emms-mode-line-only-filename ()
      "Format the currently playing song."
      (let* ((fullname (emms-track-description
                        (emms-playlist-current-selected-track)))
             (splitted (s-split "/" fullname))
             (filename (car (last splitted))))
        (concat " " (car (s-split "\\.[mp3|wma|m4a]" filename)))))
    (require 'emms-setup)
    (emms-all)
    (emms-default-players)
    (setq emms-player-mpv-parameters '("--really-quiet" "--no-audio-display" "--no-video"))
    (setq emms-source-file-default-directory "~/Music/"
          emms-playlist-buffer-name "*Music*"
          emms-info-asynchronously t)
    ;; (require 'emms-info-libtag)
    ;; (setq emms-info-functions '(emms-info-libtag))
    (require 'emms-mode-line)
    (emms-mode-line-enable)
    (emms-mode-line 1)
    (setq emms-mode-line-mode-line-function #'emms-mode-line-only-filename)
    (require 'emms-playing-time)
    (emms-playing-time nil))


  ;; python config
  (defun python-shell-send-setup-code! ()
    (interactive)
    (funcall #'python-shell-send-setup-code))
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "sS" 'python-shell-send-setup-code!)
  (setq python-test-runner 'pytest)
  (add-hook 'python-mode-hook 'code-cells-mode-maybe)


  ;; ein config
  (with-eval-after-load 'ein
    (spacemacs/set-leader-keys-for-major-mode 'ein:notebook-mode (kbd "C-r") #'ein:worksheet-execute-cell))


  (defun display-current-time ()
    "display the current time in the buffer."
    (interactive)
    (let* ((weekday-num (format-time-string "%w"))
           (weekday-str (alist-get weekday-num '(("0" . "Sun")
                                                 ("1" . "Mon")
                                                 ("2" . "Tue")
                                                 ("3" . "Wed")
                                                 ("4" . "Thu")
                                                 ("5" . "Fri")
                                                 ("6" . "Sat")) "" nil #'string-equal)))
      (message (format-time-string (concat "%Y-%m-%d %H:%M:%S " weekday-str)))))


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


  (setq grep-command "grep -R . ")


  (spacemacs/set-leader-keys "aww" 'eww)
  (spacemacs/set-leader-keys "awm" 'w3m)
  (spacemacs/set-leader-keys "wt" 'transpose-frame)
  (spacemacs/set-leader-keys "si" 'helm-imenu)
  (spacemacs/set-leader-keys "ai" 'display-current-time)
  (spacemacs/set-leader-keys "ab" 'battery)
  (spacemacs/set-leader-keys "p/" 'projectile-ripgrep)


  ;; emamux config
  (defun emamux:set-parameters- ()
    (interactive)
    (emamux:set-parameters))
  (spacemacs/declare-prefix "att" "tmux")
  (spacemacs/set-leader-keys "attt" #'emamux:send-command)
  (spacemacs/set-leader-keys "attp" #'emamux:set-parameters-)


  ;; variable-pitch-mode in these modes
  (add-hook 'elfeed-search-mode-hook #'variable-pitch-mode)
                                        ; the below two are not working...!!
  (add-hook 'latex-mode-hook #'variable-pitch-mode)
  (add-hook 'tex-mode-hook #'variable-pitch-mode)
  (add-hook 'help-mode-hook #'variable-pitch-mode)
  (add-hook 'osx-dictionary-mode-hook #'variable-pitch-mode)
  (add-hook 'hackernews-mode-hook #'variable-pitch-mode)
  (add-hook 'erc-mode #'variable-pitch-mode)


  ;; mermaid config
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")


  ;; org-mode config
  (with-eval-after-load 'org
    (setq org-startup-with-inline-images t
          org-startup-folded 'show-all
          org-startup-latex-with-latex-preview t
          org-src-fontify-natively t
          org-hide-emphasis-markers t
          org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
          org-superstar-bullet-list '("■" "◆" "▲" "▶")
          org-directory "~/Dropbox/Org")

    ;; org-capture config
    (setq org-capture-templates
          `(("w" "Work TODO" entry (file+headline ,(concat org-directory "/WorkTODO.org") "Tasks")
             "* TODO %?\n%i\n%a")
            ("W" "TODO" entry (file+headline ,(concat org-directory "/TODO.org") "Tasks")
             "* TODO %?\n%i\n%a")
            ("j" "TIL" entry (file+headline ,(concat org-directory "/TIL.org") "TIL")
             "** %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n")
            ("c" "Clipboard" entry (file+headline ,(concat org-directory "/Clipboard.org") "Clipboard")
             "** %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n")
            ("a" "Journal" entry (file+datetree,(concat org-directory "/Journal.org"))
             "** %U\n\n%?\n%i\n")
            ("s" "ShowerThoughts" entry (file+headline ,(concat org-directory "/ShowerThoughts.org") "ShowerThoughts")
             "** %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n")))

    org-todo-keywords
    '((sequence "TODO" "WORKING" "|" "DONE" "ABORTED"))
    (add-hook 'org-mode-hook #'mixed-pitch-mode)

    ;; org-roam config
    (setq org-roam-completion-everywhere t)

    ;; org-babel config
    (require 'ob-lisp)
    (require 'ob-clojure)
    (require 'ob-scheme)
    (require 'ob-hy)
    (require 'ob-dot)
    (require 'ob-rust)
    (require 'ob-kotlin)
    (require 'ob-shell)
    (require 'org-babel-no-tangle)
    (require 'org-babel-handle-imports)
    (org-babel-do-load-languages
     'org-babel-load-languages '((lisp . t) (clojure . t)
                                 (scheme . t) (hy . t)
                                 (dot . t) (rust . t)
                                 (kotlin . t) (shell . t)
                                 (mermaid . t) (plantuml . t)
                                 (awk . t)))
    (require 'org-element)
    (defun org-babel-switch-to-file ()
      (interactive)
      (let ((tangle-file-name (->> (org-babel-get-src-block-info 'light)
                                   (nth 2)
                                   (alist-get :tangle))))
        (find-file (concat (file-name-directory buffer-file-name)
                           tangle-file-name))))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "b TAB" 'org-babel-switch-to-file)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "bR" 'org-babel-remove-result)
    (setq org-src-window-setup 'current-window)
    (setq org-src-tab-acts-natively nil)

    (defun org-babel-edit-prep:kotlin (babel-info)
      (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
      (lsp))

    ;; Retina Org LaTeX Preview
    (setq org-latex-create-formula-image-program 'dvisvgm)
    (eval-after-load 'org
      '(define-key org-mode-map (kbd "H-f") 'org-latex-preview)))


  ;; split on right & below.
  (setq evil-vsplit-window-right t
        evil-split-window-below t)


  (delete-selection-mode 1)


  ;; Korean languages settings
  (set-language-environment "Korean")
  (prefer-coding-system 'utf-8)
  (global-set-key (kbd "<f6>") 'toggle-korean-input-method)


  ;; markdown-mode config
  (with-eval-after-load 'markdown-mode
    (setq markdown-command (if (string-match "aarch64.*" system-configuration)
                               "/opt/homebrew/bin/pandoc"
                               "/usr/local/bin/pandoc")))


  ;; web-mode config
  (add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))


  ;; undo-tree config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode)


  ;; Korean input method config
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
  (unbind-key (kbd "C-d"))
  (unbind-key (kbd "C-d C-d"))
  (global-set-key (kbd "C-d C-d") 'evil-toggle-input-method)
  (unbind-key (kbd "C-d C-l"))
  (global-set-key (kbd "C-d C-l") 'evil-toggle-input-method)
  (global-set-key (kbd "C-\\") 'evil-toggle-input-method)
  (global-set-key (kbd "C-d C-d") 'evil-toggle-input-method)


  ;; erc config
  (require 'erc-my-config)


  ;; graphviz config
  (with-eval-after-load 'graphviz
    (setq graphviz-dot-preview-extension "svg"))


  ;; twitch config
  (require 'twitch-api)
  (require 'twitch-my-config)
  (require 'streamlink)
  (spacemacs/set-leader-keys "awsc" 'twitch-api-erc-tls)


  ;; Listen to youtube!
  (when (executable-find "youtube-viewer")
    (defun youtube-viewer-start ()
      (interactive)
      (comint-run "youtube-viewer" '("-n")))
    (spacemacs/declare-prefix "ay" "YouTube")
    (spacemacs/set-leader-keys "ays" 'youtube-viewer-start))


  ;; mu4e config
  (when (and (executable-find "mbsync")
             (executable-find "mu"))
    (require 'mu4e-my-config))

  ) ;; user-config end

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
 '(custom-safe-themes
   '("e80b1078c4ce2225f6f8d7621a55d3b675c86cad505b22b20243d4d075f491f5" default))
 '(package-selected-packages
   '(math-symbol-lists polymode paredit markdown-mode persp-mode auctex clojure-mode closql helm-mu multiple-cursors vterm pdf-tools rust-mode compat tree-sitter-langs leetcode graphql rtags csharp-mode flycheck lsp-haskell request f s forge ghub language-id with-editor kbd-mode evil-collection company all-the-icons cider dap-mode git-commit transient helm helm-core treemacs magit-section inf-ruby ts-fold alert evil bind-key magit typescript-mode idris-mode prop-menu w3m async lsp-mode projectile multi-vterm coterm helm-spotify-plus multi spotify tao-theme modus-themes geiser-racket geiser-kawa geiser-guile geiser-gambit geiser-chicken geiser-chez 4clojure ob-async org-auto-tangle a mixed-pitch ob-rust ob-kotlin keycast command-log-mode tidal janet-mode inf-elixir inf-clojure helm-cider-history clomacs clj-deps-new clj-decompiler valign trie heap tNFA ox-asciidoc org-wild-notifier org-roam-ui org-re-reveal org-journal org-brain eshell-syntax-highlighting eshell-info-banner eshell-git-prompt eshell-did-you-mean deadgrep yaml-mode wolfram-mode vimrc-mode vala-snippets vala-mode typit mmt thrift sudoku stan-mode solidity-flycheck solidity-mode scad-mode rjsx-mode restclient-helm reddigg promise rcirc-notify rcirc-color rainbow-mode qml-mode psci purescript-mode psc-ide pony-mode pocket-reader org-web-tools rainbow-identifiers ov pocket-lib pkgbuild-mode pandoc-mode pacmacs ox-rfc ox-pandoc ob-restclient ob-http ob-elixir nim-mode flycheck-nimsuggest commenter matlab-mode lsp-julia lsp-dart logcat kubernetes-tramp kubernetes-evil kubernetes magit-popup julia-repl julia-mode ietf-docs hoon-mode helpful elisp-refs streamlink graphql-mode git-gutter-fringe fringe-helper git-gutter fstar-mode company-quickhelp quick-peek flycheck-nim flycheck-ledger flycheck-credo flutter evil-snipe evil-mc evil-ledger ledger-mode erlang ediprolog ebuild-mode dotnet djvu3 djvu direx dired-k dart-mode dactyl-mode copy-as-format company-restclient restclient know-your-http-well color-identifiers-mode browse-at-remote bm better-jumper arduino-mode alchemist elixir-mode adoc-mode markup-faces 2048-game soundklaus erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks ess-R-data-view ess citar citeproc sql-indent org-roam-bibtex bibtex-completion biblio parsebib biblio-core org-noter org-listcruncher org-link-beautify org-kanban company-lua lua-mode fennel-mode kotlin-mode flycheck-kotlin web-server yasnippet-snippets yapfify xwwp xterm-color ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vmd-mode vi-tilde-fringe uuidgen utop use-package undo-tree ts treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil transpose-frame toml-mode toc-org terminal-here tagedit symon symbol-overlay string-inflection string-edit sphinx-doc spaceline-all-the-icons smeargle slime-company slim-mode sicp shell-pop seeing-is-believing scss-mode sbt-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode ron-mode robe rg reveal-in-osx-finder restart-emacs rbenv rake rainbow-delimiters racket-mode racer quickrun python-pytest pytest pyenv-mode pydoc py-isort pug-mode proof-general prettier-js poetry plantuml-mode pippel pipenv pip-requirements pdf-view-restore password-generator paradox overseer osx-trash osx-dictionary osx-clipboard orgit-forge org-superstar org-roam org-rich-yank org-projectile org-present org-pomodoro org-mime org-download org-contrib org-cliplink open-junk-file omnisharp ocp-indent ocamlformat ob-mermaid ob-hy npm-mode nov nose nodejs-repl nameless mvn multi-term multi-line mmm-mode minitest mermaid-mode merlin-iedit merlin-eldoc merlin-company maven-test-mode markdown-toc lsp-ui lsp-rescript lsp-python-ms lsp-pyright lsp-origami lsp-metals lsp-latex lsp-java lsp-docker lorem-ipsum livid-mode live-py-mode launchctl json-reformat json-navigator js2-refactor js-doc inspector info+ indent-guide importmagic impatient-mode ibuffer-projectile hybrid-mode hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt helm-xref helm-w3m helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-hoogle helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-cider helm-c-yasnippet helm-ag haskell-snippets hackernews groovy-mode groovy-imports graphviz-dot-mode google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link gh-md gendoxy geiser fuzzy fsharp-mode format-all font-lock+ flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-ocaml flycheck-haskell flycheck-elsa flycheck-elm flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-tex evil-terminal-cursor-changer evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help eradio emr emoji-cheat-sheet-plus emms emmet-mode emamux elm-test-runner elm-mode elisp-slime-nav elisp-def elfeed-org elfeed-goodies ein editorconfig dune dumb-jump drag-stuff dotenv-mode doom-modeline dockerfile-mode docker disaster dired-quick-sort diminish devdocs dante cython-mode csv-mode cpp-auto-include company-ycmd company-web company-rtags company-reftex company-plsense company-go company-emoji company-coq company-cabal company-c-headers company-auctex company-anaconda common-lisp-snippets column-enforce-mode code-cells cmm-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode ccls cargo bundler blacken auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk attrap aggressive-indent ag ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "#C0FFEE"))))
 '(font-lock-keyword-face ((t (:foreground "#5317ac" :weight bold))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(lsp-face-highlight-textual ((t (:weight bold :underline t))))
 '(org-code ((t (:font "Courier"))))
 '(tool-bar ((t (:background "#d7d7d7")))))
)
