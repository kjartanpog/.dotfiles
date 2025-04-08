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

(use-package diminish
  :straight t
  :config)

(use-package emacs
  :init                                           ;; Initialization settings that apply before the package is loaded.
  (setq inhibit-startup-screen t)                 ;; Inhibits the default welcome to emacs startup screen.
  (tool-bar-mode -1)                              ;; Disable the tool bar for a cleaner interface.
  ;; (scroll-bar-mode -1)                            ;; Disable the scroll bar for a cleaner interface.
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
  ;; (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  ;; (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . electric-pair-local-mode)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . (lambda () (setq-local truncate-lines t)))         ;; Enable line numbers in programming modes.

  ;; :bind (("C-+" . text-scale-increase)
  ;; 	 ("C--" . text-scale-decrease)
  ;; 	 ("C-x C-r" . recentf))
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
  (set-window-scroll-bars (minibuffer-window) nil nil nil nil 1)
  (set-window-parameter (get-buffer-window "*Messages*") 'vertical-scroll-bars nil)
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-type 'relative) ; Set relative line numbers
  (setq vc-follow-symlinks t) ; Always follow symbolic link to a file under version control.
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

  (setq word-wrap t)

  ;; :TODO: Move to use-package org
  (defun my/enable-visual-line-mode-and-wrap ()
    "Enable visual line mode and set word wrap in non-programming modes."
    (visual-line-mode 1)
    ;; (org-indent-mode 1)
    (variable-pitch-mode 1))
  ;; (add-hook 'text-mode-hook 'my/enable-visual-line-mode-and-wrap)
  (add-hook 'org-mode-hook 'my/enable-visual-line-mode-and-wrap)
  ;; (add-hook 'markdown-mode-hook 'my/enable-visual-line-mode-and-wrap)
  ;; (add-hook 'message-mode-hook 'my/enable-visual-line-mode-and-wrap)
  )

;;; Modeline

(use-package project
  :custom
  (project-mode-line t))

(use-package eldoc
  :diminish eldoc-mode)

;;; Bookmarks, History & Undo

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

;;; Minibuffer

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


;;; Keymaps

(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode t))

;;;; evil
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil) ;; Disable loading a set of keybindings for evil in other modes (using evil-collection instead)
  (setq evil-want-integration t)
  (setq evil-respect-visual-line-mode t)        ;; Whether movement commands respect ‘visual-line-mode’.
  :custom
  (evil-want-C-u-scroll t)                 ;; Makes ‘C-u’ scroll up (like Vim).
  (evil-want-C-u-delete t)                 ;; Makes ‘C-u’ delete on insert mode
  (evil-split-window-below t)              ;; Horizontally split windows are created below.
  (evil-vsplit-window-right t)             ;; Vertically split windows with are created to the right.
  (evil-respect-visual-line-mode t)        ;; Whether movement commands respect ‘visual-line-mode’.
  (evil-undo-system 'undo-fu)
  (evil-toggle-key "C-M-z")           ;; Toggle between emacs and vim bindings with ‘C-u’
  :config
  (evil-mode t)
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file))
  (add-hook 'evil-insert-state-entry-hook
	    (lambda ()
	      (when (derived-mode-p 'prog-mode)
		(setq display-line-numbers-type t)
		;; (hl-line-mode -1)
		(display-line-numbers-mode -1)
		(display-line-numbers-mode 1))))
  (add-hook 'evil-insert-state-exit-hook
	    (lambda ()
	      (when (derived-mode-p 'prog-mode)
		(setq display-line-numbers-type 'relative)
		;; (hl-line-mode 1)
		(display-line-numbers-mode -1)
		(display-line-numbers-mode 1)))))

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

(use-package evil-snipe
  :straight t
  :after evil
  :diminish (evil-snipe-local-mode)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)

  ;; and disable in specific modes
  ;; (push 'python-mode evil-snipe-disabled-modes)

  ;; or disable it manually
  ;; (add-hook 'python-mode-hook #'turn-off-evil-snipe-mode)
  ;; (add-hook 'python-mode-hook #'turn-off-evil-snipe-override-mode)
  )

(use-package evil-textobj-tree-sitter
  :straight t
  :after (evil evil-collection)
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  )
  
;;;; general
(use-package general
  :straight t
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    ;; Execute / Commands
    "<escape>" '(keyboard-escape-quit :which-key t)
    ":" '(execute-extended-command :which-key "execute command")

    ;; File Searching
    "f"  '(:ignore t :which-key "File")
    "f <escape>" '(keyboard-escape-quit :which-key t)
    "fi" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")
    "ff"  '(find-file :which-key t)
    "fr"  '(recentf :which-key t)

    ;; Buffer
    "b" '(:ignore t :which-key "Buffer")
    "b <escape>" '(keyboard-escape-quit :which-key t)
    ;; "bk"  '(kill-current-buffer)
    "bk"  '(kill-current-buffer :which-key "Kill Current")
    "bn"  '(next-buffer :which-key "Next")
    "bp"  '(previous-buffer :which-key "Previous")
    "bf"  '(consult-buffer :which-key "Find")

;;;; general s
    "s" '(:ignore t :which-key "Search")
    "s <escape>" '(keyboard-escape-quit :which-key t)
    "sr" '(consult-ripgrep :which-key "ripgrep")
    "so" '(consult-outline :which-key "outline")
    "sl" '(consult-line :which-key "line")
    "su" '(vundo :which-key t "undo")
    "st" '(consult-todo-project :which-key t "todo")

    ;; Window Navigation
    "w" '(:ignore t :which-key "window")
    "w <escape>" '(keyboard-escape-quit :which-key t)
    "wl" '(evil-window-right :which-key "right")
    "wh" '(evil-window-left :which-key "left")
    "wk" '(evil-window-up :which-key "up")
    "wj" '(evil-window-down :which-key "down")
    "wL" '(evil-window-move-far-right :which-key "move right")
    "wH" '(evil-window-move-far-left :which-key "move left")
    "wK" '(evil-window-move-very-top :which-key "move top")
    "wJ" '(evil-window-move-very-bottom :which-key "move bottom")
    "wr" '(evil-window-rotate-downwards :which-key "rotate")
    "ws" '(evil-window-split :which-key "split horizontally")
    "wv" '(evil-window-vsplit :which-key "split vertically")
    "wc" '(evil-window-delete :which-key "delete")
    "wq" '(evil-quit :which-key "quit")
    "wt" '(tab-new :which-key "new tab")
    "wu" '(winner-undo :which-key "undo")

     ;; Help
    "h" '(:ignore t :which-key "Help")
    "h <escape>" '(keyboard-escape-quit :which-key t)
    "hf" '(helpful-callable :which-key "Callable")
    "hv" '(helpful-variable :which-key "Variable")
    "hk" '(helpful-key :which-key "Key")
    "hx" '(helpful-command :which-key "Command")
    "hd" '(helpful-at-point :which-key "At point")
    "hF" '(helpful-function :which-key "Function")
    "ho" '(helpful-symbol :which-key "Symbol")
    "hm" '(describe-mode :which-key "Major mode")
    "hM" '(describe-minor-mode :which-key "Minor mode")
    "hp" '(describe-package :which-key "Package")

     ;; Bookmark / Recent
    "r" '(:ignore t :which-key "Recent")
    "r <escape>" '(keyboard-escape-quit :which-key t)
    "rb" '(bookmark-jump :which-key t)
    "rm" '(bookmark-set :which-key t)
    "rl" '(bookmark-bmenu-list :which-key t)
    "ru" '(vundo :which-key t)

     ;; Project
    "p" '(:ignore t :which-key "Project")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "pf" '(project-find-file :which-key t)

    ;; LLM
    "l" '(:ignore t :which-key "LLM")
    "l <escape>" '(keyboard-escape-quit :which-key t)
    "ll" '(gptel :which-key "gptel")

     ;; Org Roam
    "n" '(:ignore t :which-key "Org Roam")
    "n <escape>" '(keyboard-escape-quit :which-key t)
    ;; "nf" '(org-roam-node-find :which-key "node find")
    ;; "ni" '(org-roam-node-insert :which-key "node insert")
    ;; "nc" '(org-roam-capture :which-key "capture")
    ;; "ng" '(org-roam-graph :which-key "graph")
    ;; "nl" '(org-roam-buffer-toggle :which-key t)
    ;; "nj" '(org-roam-dailies-capture-today :which-key t)

;;;; general t
    "t" '(:ignore t :which-key "Toggle")
    "t <escape>" '(keyboard-escape-quit :which-key t)
    "tt" '(modus-themes-toggle :which-key "Theme")
    "tr" '(rainbow-mode  :which-key "Rainbow")
    "tl" '(toggle-truncate-lines :which-key "truncate lines")

;;;; general g
    "g" '(:ignore t :which-key "Toggle")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "gs" '(magit-status :which-key "Status")
    "gn" '(diff-hl-next-hunk :which-key "Next Hunk")
    "gp" '(diff-hl-previous-hunk :which-key "Previous Hunk")
  ))

(use-package expreg
  :straight t
  :bind (("C-+" . expreg-expand)
	 ("C--" . expreg-contract)))

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

  (setq modus-themes-headings
	'((0 . (1.35))
          (1 . (1.30))
          (2 . (1.24))
          (3 . (semibold 1.17))
          (4 . (1.14))
          (t . (monochrome))))

  ;; Maybe define some palette overrides, such as by using our presets
  ;;(setq modus-themes-common-palette-overrides
  ;;      modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  ;; (load-theme 'modus-operandi :no-confirm)
  (load-theme 'modus-vivendi))

(use-package rainbow-mode
  :straight t)

(use-package spacious-padding
  :straight t
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 2
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))
  (spacious-padding-mode 1)
  )

(use-package visual-fill-column
  :straight t
  :defer t
  :custom
  (visual-fill-column-width 80)
  :hook (org-mode . (lambda ()
		      (visual-fill-column-mode)
		      ;; (visual-line-fill-column-mode)
		      (visual-fill-column-toggle-center-text)))
  )

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
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-preselect 'first)
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

(use-package consult-todo
  :straight t
  :after (consult))

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
  ;; :disabled
  :straight t
  ;; :after treesit
  :config
  (setq treesit-load-name-override-list
	'((python "python" "tree_sitter_python")
	  (nix "nix" "tree_sitter_nix")))
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

;; (use-package org-roam
;;   :straight t
;;   :custom
;;   (org-roam-directory "~/org/roam")
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode))

;; Terminal Emacs

(use-package evil-terminal-cursor-changer
  :straight t
  :if (not (display-graphic-p))
  :config
  (evil-terminal-cursor-changer-activate))

;; (use-package combobulate
;;   :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
;;   :defer t
;;   )

;; AI

(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode #'org-mode))

(use-package indent-bars
  :straight t
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  ;; :hook ((python-ts-mode yaml-mode) . indent-bars-mode)
  :hook (((python-base-mode yaml-mode) . indent-bars-mode)
	 (nix-ts-mode . indent-bars-mode)))

(use-package treesit-fold
  :straight t
  :config
  (setq treesit-fold-line-count-show t
	treesit-fold-line-count-format " <%d lines> ")
  (global-treesit-fold-mode))


;; Version Control
(use-package magit
  :straight t)

(use-package diff-hl 
  :straight t
  :after (magit)
  :config
  (global-diff-hl-mode)
  (global-diff-hl-show-hunk-mouse-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (custom-set-faces
   '(diff-hl-insert ((t (:background "#88ca9f" :foreground "#092f1f"))))
   '(diff-hl-delete ((t (:background "#ff7f86" :foreground "#3a0c14"))))
   '(diff-hl-change ((t (:background "#dfaf7a" :foreground "#381d0f")))))
)


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

(use-package org-indent
  :config
  ;; In order to avoid line spacing issues when a line of text
  ;; contains both variable- and fixed-pitch text, we need to
  ;; make sure that the org-indent face inherits from fixed-pitch.
  ;; (set-face-attribute 'org-indent nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch :height 0.85)
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :height 0.85)
  ;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch) :height 0.85)
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :height 0.85)
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  )
(use-package org-download
  :straight t
  :config

  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (add-hook 'org-mode-hook 'org-download-enable)
  )

(use-package ultra-scroll
  :straight (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  ;; :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of using vc
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(use-package nerd-icons
  :straight t
  :config
  (custom-set-faces
   '(nerd-icons-folder ((t (:foreground "#008899"))))
   '(nerd-icons-folder-open ((t (:foreground "#008899"))))
   ;; Add more customizations as needed for other icons
   )
  )

(use-package nerd-icons-dired
  :straight t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :straight t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (custom-set-faces
   '(nerd-icons-completion-dir-face ((t (:foreground "#008899"))))
  ))

(use-package nerd-icons-corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

  ;; Optionally:
  ;; (setq nerd-icons-corfu-mapping
  ;; 	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
  ;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
  ;;         ;; You can alternatively specify a function to perform the mapping,
  ;;         ;; use this when knowing the exact completion candidate is important.
  ;;         (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
  ;;         ;; ...
  ;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.

  ;; The Custom interface is also supported for tuning the variable above.
  )


 (use-package pulsar
  :straight t
  :config
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-backward)
  (setq pulsar-pulse-functions (remove 'evil-scroll-up pulsar-pulse-functions))
  (setq pulsar-pulse-functions (remove 'evil-scroll-down pulsar-pulse-functions))
  (pulsar-global-mode))

(diminish 'auto-revert-mode)
;; (load "./custom.el")
(progn

  ;; set font for emoji
  ;; if before emacs 28, this should come after setting symbols, because emacs 28 now has 'emoji . before, emoji is part of 'symbol

  (set-fontset-font
   t
   (if (< emacs-major-version 28)
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

(use-package qrencode
  :straight t) 

(use-package eglot
  :config
  (add-hook 'nix-ts-mode-hook 'eglot-ensure)

  (defun my-cape-complete-eglot ()
    (when (and (eglot-managed-p) (bound-and-true-p eglot--current-server))
      (eglot-completion-at-point)))

  ;; Add Eglot completions to cape
  (add-to-list 'completion-at-point-functions #'my-cape-complete-eglot)
  )

;;; Note taking

(use-package denote
  :straight t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/Denote"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

	      


;;; Organize later
;; (use-package dashboard
;;   :straight t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   :init
;;   (setq dashboard-items '((recents   . 5)
;; 			  (bookmarks . 5)
;; 			  (projects  . 5)
;; 			  (agenda    . 5)
;; 			  (registers . 5)))
;;   ;; (setq dashboard-center-content t)
;;   (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
;;   ;; To add icons to the widget headings and their items:
;;   )

;;; Get shit done
(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode))

(diminish 'auto-revert-mode)
(diminish 'treesit-fold-mode)
(diminish 'outline-minor-mode)
(put 'dired-find-alternate-file 'disabled nil)
