;;; -*- lexical-binding: t -*-
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package emacs
  :init                                           ;; Initialization settings that apply before the package is loaded.
  (setq inhibit-startup-screen t)                 ;; Inhibits the default welcome to emacs startup screen.
  (tool-bar-mode -1)                              ;; Disable the tool bar for a cleaner interface.
  (scroll-bar-mode -1)                            ;; Disable the scroll bar for a cleaner interface.
  (savehist-mode 1)                               ;; Enable saving of command history.
  (save-place-mode 1)                             ;; Enable saving the place in files for easier return.
  (line-number-mode -1)                           ;; Disable display of line number in the mode-line
  (winner-mode 1)                                 ;; Enable winner mode to easily undo window configuration changes.
  (modify-coding-system-alist 'file "" 'utf-8)    ;; Set the default coding system for files to UTF-8.

  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (custom-safe-themes t)
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (tab-always-indent 'complete) ;; TAB first tries to indent the current line, and if the line was already indented, then try to complete the thing at point.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)
  (prog-mode . (lambda () (setq-local truncate-lines t)))         ;; Enable line numbers in programming modes.

  :bind (("C-+" . text-scale-increase)
	 ("C--" . text-scale-decrease)
	 ("C-x C-r" . recentf))
  ;; TODO - Can I get this integrated with :map ?
  ;; (global-set-key (kbd "C-+") 'text-scale-increase)
  ;; (global-set-key (kbd "C--") 'text-scale-decrease)
  
  :config
  (context-menu-mode 1)                           ;; Enable right click mouse menu.
  ;;TODO consider evil emacs mode hook
  ;;(cua-mode 1)                                    ;; use C-z, C-x, C-c, and C-v to undo, cut, copy, and paste
  (recentf-mode t)                                ;; Enable tracking of recently opened files.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p (concat user-emacs-directory "custom.el"))
    (load (concat user-emacs-directory "custom.el")))
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/"))))
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups/") t)))
  
  )

;; Modeline

(use-package diminish
  :straight t)

(use-package project
  :custom
  (project-mode-line t))

(use-package eldoc
  :diminish eldoc-mode)

;; Bookmarks, History & Undo

(use-package bookmark
 :config
 (setopt bookmark-save-flag 1)
 ;; (run-at-time nil (* 5 60) #'bookmark-save)
 )

(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t
  :config
  ;; (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
  (undo-fu-session-global-mode))

(use-package vundo
  :straight t
  ;; :general
  ;; (:keymaps 'vundo-mode-map
  ;; 	    :states '(normal insert visual)
  ;; 	    "<escape>" 'vundo-quit)
  :bind (:map vundo-mode-map
              ("<escape>" . vundo-quit)))

;; Minibuffer

(use-package vertico
  :straight t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  ;; :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :after vertico
  :config
  (vertico-mouse-mode t))
  

(use-package orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package prescient
  :straight t)

(use-package vertico-prescient
  :straight t
  :after (prescient vertico)
  :config
  (vertico-prescient-mode t))

(use-package corfu-prescient
  :straight t
  :after (prescient corfu)
  :config
  (corfu-prescient-mode t))


;; Keymaps

(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :config
  (which-key-mode t))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil ;; Disable loading a set of keybindings for evil in other modes (using evil-collection instead)
	evil-want-integration t)
  :custom
  (evil-want-C-u-scroll t)                 ;; Makes ‘C-u’ scroll up (like Vim).
  (evil-want-C-u-delete t)                 ;; Makes ‘C-u’ delete on insert mode
  (evil-split-window-below t)              ;; Horizontally split windows are created below.
  (evil-vsplit-window-right t)             ;; Vertically split windows with are created to the right.
  (evil-respect-visual-line-mode t)        ;; Whether movement commands respect ‘visual-line-mode’.
  (evil-undo-system 'undo-fu)
  ;; (evil-toggle-key "C-M-z")           ;; Toggle between emacs and vim bindings with ‘C-u’
  :config
  ;; Define the leader key as Space
  (evil-set-leader 'normal (kbd "SPC")) 
  (evil-set-leader 'visual (kbd "SPC")) 

  ;; File Searching
  (evil-define-key 'normal 'global (kbd "<leader> f f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader> f r") 'recentf)

  ;; Window Navigation
  (evil-define-key 'normal 'global (kbd "<leader> w l") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader> w h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader> w k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "<leader> w j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "<leader> w r") 'evil-window-rotate-downwards)
  (evil-define-key 'normal 'global (kbd "<leader> w s") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader> w v") 'evil-window-vsplit)
  (evil-define-key 'normal 'global (kbd "<leader> w c") 'evil-window-delete)
  (evil-define-key 'normal 'global (kbd "<leader> w q") 'evil-quit)
  (evil-define-key 'normal 'global (kbd "<leader> w u") 'winner-undo)

  ;; Help
  (evil-define-key 'normal 'global (kbd "<leader> h f") 'helpful-callable)
  (evil-define-key 'normal 'global (kbd "<leader> h v") 'helpful-variable)
  (evil-define-key 'normal 'global (kbd "<leader> h k") 'helpful-key)
  (evil-define-key 'normal 'global (kbd "<leader> h x") 'helpful-command)
  (evil-define-key 'normal 'global (kbd "<leader> h d") 'helpful-at-point)
  (evil-define-key 'normal 'global (kbd "<leader> h F") 'helpful-function)

  ;; Bookmark / Recent
  (evil-define-key 'normal 'global (kbd "<leader> r b") 'bookmark-jump)
  (evil-define-key 'normal 'global (kbd "<leader> r m") 'bookmark-set)
  (evil-define-key 'normal 'global (kbd "<leader> r l") 'bookmark-bmenu-list)

  ;; Project
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file)

  ;; Org Roam
  (evil-define-key 'normal 'global (kbd "<leader> n f") 'org-roam-node-find)
  (evil-define-key 'normal 'global (kbd "<leader> n i") 'org-roam-node-insert)
  (evil-define-key 'normal 'global (kbd "<leader> n c") 'org-roam-capture)
  (evil-define-key 'normal 'global (kbd "<leader> n g") 'org-roam-graph)
  (evil-define-key 'normal 'global (kbd "<leader> n l") 'org-roam-buffer-toggle)
  (evil-define-key 'normal 'global (kbd "<leader> n j") 'org-roam-dailies-capture-today)

  ;; Show UI / Elements
  (evil-define-key 'normal 'global (kbd "<leader> s u") 'vundo)

  ;; Toggle UI / Elements
  (evil-define-key 'normal 'global (kbd "<leader> t t") 'modus-themes-toggle)

  ;; Execute commands
  (evil-define-key 'normal 'global (kbd "<leader> :") 'execute-extended-command)
  


  (evil-mode t))

(use-package evil-collection
  :straight t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :custom
  (evil-collection-setup-minibuffer t) ;; Setup ‘evil’ bindings in the ‘minibuffer’
  (evil-collection-which-key-setup t) ;; Setup ‘evil’ bindings for ‘which-key’.
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil-collection
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode t))

(use-package evil-surround
  :straight t
  :after evil-collection
  :diminish global-evil-surround-mode
  :config
  (global-evil-surround-mode t))

;; Fonts & Theme

(use-package modus-themes
  :straight t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  (setq modus-vivendi-palette-overrides
	`((bg-main "#090909")
	  (fg-heading-1 magenta-faint)))

  (setq modus-operandi-palette-overrides
        '((fg-heading-1 "#2f5f9f")))

  ;; Maybe define some palette overrides, such as by using our presets
  ;;(setq modus-themes-common-palette-overrides
  ;;      modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  ;; (load-theme 'modus-operandi :no-confirm)
  (load-theme 'modus-vivendi))

(use-package vi-tilde-fringe
  :straight t
  :diminish vi-tilde-fringe-mode
  :hook (prog-mode-hook . vi-tilde-fringe-mode))

(use-package auto-dark
  :straight t
  :diminish auto-dark-mode
  :if (display-graphic-p)
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :init (auto-dark-mode)) 



;; Info / Documentation

(use-package helpful
  :straight t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

;; Performance

(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :hook
  (after-init-hook . gcmh-mode))

;; Completions

    ;; In buffer

(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  :bind
  (:map corfu-map
        ("TAB" . corfu-next) ;; Use TAB for cycling to the next candidate
        ([tab] . corfu-next) ;; Ensure both TAB and [tab] work
        ("S-TAB" . corfu-previous) ;; Use Shift-TAB for cycling to the previous candidate
        ([backtab] . corfu-previous)
	("SPC" . corfu-insert-separator))

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :straight t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

    ;; Minibuffer

;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
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

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Treesitter Programming Modes & More

(use-package treesit
  ;; :after tree-sitter-langs
  :config
  (setq treesit-language-source-alist
   '((nix . ("https://github.com/nix-community/tree-sitter-nix" "v0.0.2"))))
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  )

(use-package tree-sitter-langs
  :disabled
  :straight t
  ;; :after treesit
  :config
  (setq treesit-extra-load-path
	(list tree-sitter-langs--dir
	      (concat tree-sitter-langs--dir "bin/")))
  ;; (setq treesit-load-name-override-list
  ;;     (list (list "python" "python" "python")))
)

(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")

(use-package nix-ts-mode
  :straight t
  :if (treesit-language-available-p 'nix)
  :defer t
  :after (treesit)
  :init
  (setq major-mode-remap-alist
	(append major-mode-remap-alist
		'((nix-mode . nix-ts-mode)))))

(use-package conf-mode
  :straight nil ;; builtin
  :mode "\\.inputrc\\'"
  :hook
  (conf-mode . display-line-numbers-mode)
  (conf-mode . (lambda () (setq-local truncate-lines t)))         ;; Enable line numbers 
  )
  

(use-package python
  :init
  (if (treesit-language-available-p 'python)
      (setq major-mode-remap-alist
	    (append major-mode-remap-alist
		    '((python-mode . python-ts-mode))))))

;; Org Mode

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory "~/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

;; Terminal Emacs

(use-package evil-terminal-cursor-changer
  :straight t
  :if (not (display-graphic-p))
  :config
  (evil-terminal-cursor-changer-activate))

(cond

 ((when (member "Aporetic Sans Mono" (font-family-list))

    ;;(set-frame-font "Aporetic Sans Mono 14" t t)
    (set-face-attribute 'default nil
                    :family "Aporetic Sans Mono"
                    :height 140
                    :weight 'normal
                    :width 'normal)
    (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 160)
    )))
