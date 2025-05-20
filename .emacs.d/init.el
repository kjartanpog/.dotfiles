;;; -*- lexical-binding: t -*-

;; [[file:emacs.org::*init.el][init.el:2]]
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

;; (straight-use-package 'org)
(straight-use-package 'use-package)
(setq straight-built-in-pseudo-packages '(org))
(use-package org
  :straight nil)
;; init.el:2 ends here

;; [[file:emacs.org::*defun.el][defun.el:2]]
;; (load (expand-file-name "defun.el" user-emacs-directory))
(load (expand-file-name "defun.el"
                        (file-name-directory load-file-name)))
;; defun.el:2 ends here

;; [[file:emacs.org::*defvar.el][defvar.el:2]]
;; (load (expand-file-name "defvar.el" user-emacs-directory))
(load (expand-file-name "defvar.el"
                        (file-name-directory load-file-name)))
;; defvar.el:2 ends here

;; (load (expand-file-name "org-latex-preview.el" user-emacs-directory))
;; (load (expand-file-name "org-latex-preview.el"
;;                        (file-name-directory load-file-name)))
;; (use-package org)  
;; (require 'org)

;; [[file:emacs.org::*custom.el][custom.el:1]]
(use-package emacs
  :config
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p (concat user-emacs-directory "custom.el"))
    (load (concat user-emacs-directory "custom.el"))))
;; custom.el:1 ends here

;; [[file:emacs.org::*File Encoding][File Encoding:1]]
(use-package emacs
  :config
  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8))
;; File Encoding:1 ends here

;; [[file:emacs.org::*indent-bars][indent-bars:1]]
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
;; indent-bars:1 ends here

;; [[file:emacs.org::*Line Numbers, Wrapping & More][Line Numbers, Wrapping & More:1]]
(use-package emacs
  :config
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-type 'relative) ; Set relative line numbers
  (add-hook 'prog-mode-hook '(lambda () (hl-line-mode 1)))
  )
;; Line Numbers, Wrapping & More:1 ends here

;; [[file:emacs.org::*Hybrid line numbers][Hybrid line numbers:1]]
(defun my/hybrid-line-numbers-evil-insert-state-entry ()
  "Swap to regular line numbers if inside prog-mode"
  (when (derived-mode-p 'prog-mode)
    (setq display-line-numbers-type t)
    (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)
    (hl-line-mode -1)))

(defun my/hybrid-line-numbers-evil-insert-state-exit ()
  "Swap to relative line numbers if inside prog-mode"
  (when (derived-mode-p 'prog-mode)
    (setq display-line-numbers-type 'relative)
    (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)
    (hl-line-mode 1)))
;; Hybrid line numbers:1 ends here

;; [[file:emacs.org::*Hybrid line numbers][Hybrid line numbers:2]]
(add-hook 'evil-insert-state-entry-hook
	  #'my/hybrid-line-numbers-evil-insert-state-entry)
(add-hook 'evil-insert-state-exit-hook
	  #'my/hybrid-line-numbers-evil-insert-state-exit)
;; Hybrid line numbers:2 ends here

;; [[file:emacs.org::*For Programming][For Programming:1]]
(use-package emacs
  :hook
  (prog-mode . electric-pair-local-mode)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . (lambda () (setq-local truncate-lines t)))
  )
;; For Programming:1 ends here

;; [[file:emacs.org::*For Word Processing][For Word Processing:1]]
(use-package emacs
  :config
  (setq word-wrap t)

  (defun my/enable-visual-line-mode-and-wrap ()
    "Enable visual line mode and set word wrap in non-programming modes."
    (visual-line-mode 1)
    (variable-pitch-mode 1))
  (add-hook 'org-mode-hook 'my/enable-visual-line-mode-and-wrap)
  )
;; For Word Processing:1 ends here

;; [[file:emacs.org::*Changing annoying defaults][Changing annoying defaults:1]]
(use-package emacs
  :config
  (setq

   ;; Disable the audible bell
   ring-bell-function 'ignore

   ;; Use short answers in prompts (y instead of yes)
   use-short-answers t

   ;; Inhibits the default welcome to emacs startup screen
   inhibit-startup-screen t

   ;; Automatically refresh non-file buffers.
   global-auto-revert-non-file-buffers t

   )

  ;; Disable the tool bar for a cleaner interface
  (tool-bar-mode -1))
;; Changing annoying defaults:1 ends here

;; [[file:emacs.org::*Bookmarks][Bookmarks:1]]
(use-package bookmark
  :config
  (setopt bookmark-save-flag 1)
  ;; (run-at-time nil (* 5 60) #'bookmark-save)
  )
;; Bookmarks:1 ends here

;; [[file:emacs.org::*undo-fu][undo-fu:1]]
(use-package undo-fu
  :straight t)
;; undo-fu:1 ends here

;; [[file:emacs.org::*undo-fu-session][undo-fu-session:1]]
(use-package undo-fu-session
  :straight t
  :config
  ;; (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
  (undo-fu-session-global-mode))
;; undo-fu-session:1 ends here

;; [[file:emacs.org::*vundo][vundo:1]]
(use-package vundo
  :straight t
  ;; :general
  ;; (:keymaps 'vundo-mode-map
  ;; 	    :states '(normal insert visual)
  ;; 	    "<escape>" 'vundo-quit)
  :config
  (setq vundo-glyph-alist vundo-ascii-symbols)
  (advice-add 'vundo-forward :after (lambda (&rest _args) (recenter)))
  (advice-add 'vundo-backward :after (lambda (&rest _args) (recenter)))
  :bind (:map vundo-mode-map
	      ("<escape>" . vundo-quit)))
;; vundo:1 ends here

;; [[file:emacs.org::*Minibuffer][Minibuffer:1]]
(use-package emacs
  :init
  ;; Save minibuffer history to `savehist-file' periodically and when exiting Emacs.
  (savehist-mode 1))
;; Minibuffer:1 ends here

;; [[file:emacs.org::*Remember last location in file][Remember last location in file:1]]
(use-package emacs
  :init
  ;; Enable saving the place in files for easier return
  (save-place-mode 1))
;; Remember last location in file:1 ends here

;; [[file:emacs.org::*Recently opened files][Recently opened files:1]]
(use-package emacs
  :config
  ;; Enable tracking of recently opened files.
  (recentf-mode t))
;; Recently opened files:1 ends here

;; [[file:emacs.org::*Automatic backups][Automatic backups:1]]
(use-package emacs
  :config
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/"))))
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups/") t))))
;; Automatic backups:1 ends here

;; [[file:emacs.org::*The TAB key][The TAB key:1]]
(use-package emacs
  ;; TAB first tries to indent, then complete thing at point
  :config
  (setq tab-always-indent 'complete))
;; The TAB key:1 ends here

;; [[file:emacs.org::*vertico][vertico:1]]
(use-package vertico
  :straight t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))
;; vertico:1 ends here

;; [[file:emacs.org::*vertico-directory][vertico-directory:1]]
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
;; vertico-directory:1 ends here

;; [[file:emacs.org::*vertico-mouse][vertico-mouse:1]]
(use-package vertico-mouse
  :after vertico
  :config
  (vertico-mouse-mode t))
;; vertico-mouse:1 ends here

;; [[file:emacs.org::*marginalia][marginalia:1]]
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
;; marginalia:1 ends here

;; [[file:emacs.org::*consult][consult:1]]
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
;; consult:1 ends here

;; [[file:emacs.org::*consult-todo][consult-todo:1]]
(use-package consult-todo
  :straight t
  :after (consult))
;; consult-todo:1 ends here

;; [[file:emacs.org::*Mode-specific context][Mode-specific context:1]]
(use-package emacs
  :config
  ;; Hide commands in M-x which do not apply to the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
;; Mode-specific context:1 ends here

;; [[file:emacs.org::*corfu][corfu:1]]
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
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))
;; corfu:1 ends here

;; [[file:emacs.org::*cape][cape:1]]
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
;; cape:1 ends here

;; [[file:emacs.org::*embark][embark:1]]
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
;; embark:1 ends here

;; [[file:emacs.org::*embark-consult][embark-consult:1]]
(use-package embark-consult
  :after embark
  :straight t)
;; embark-consult:1 ends here

;; [[file:emacs.org::*orderless][orderless:1]]
(use-package orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
;; orderless:1 ends here

;; [[file:emacs.org::*prescient][prescient:1]]
(use-package prescient
  :disabled t
  :straight t)
;; prescient:1 ends here

;; [[file:emacs.org::*vertico-prescient][vertico-prescient:1]]
(use-package vertico-prescient
  :disabled t
  :straight t
  :after (prescient vertico)
  :config
  (vertico-prescient-mode t))
;; vertico-prescient:1 ends here

;; [[file:emacs.org::*corfu-prescient][corfu-prescient:1]]
(use-package corfu-prescient
  :disabled t
  :straight t
  :after (prescient corfu)
  :config
  (corfu-prescient-mode t))
;; corfu-prescient:1 ends here

;; [[file:emacs.org::*yasnippet][yasnippet:1]]
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode t))
;; yasnippet:1 ends here

(use-package dired
  :config
  (add-hook 'dired-mode-hook
        (lambda ()
          (dired-hide-details-mode))))

;; [[file:emacs.org::*/[/[https:/github.com/protesilaos/fontaine/]/[fontaine/]/]][[[https://github.com/protesilaos/fontaine][fontaine]]:1]]
(use-package fontaine
:straight t
:config
(locate-user-emacs-file "fontaine-latest-state.eld")
(setq fontaine-presets
	'((small
	   :default-family "Aporetic Serif Mono"
	   :default-height 80
	   :variable-pitch-family "Aporetic Sans")
	  (regular) ; like this it uses all the fallback values and is named `regular'
	  (medium
	   :default-weight semilight
	   :default-height 115
	   :bold-weight extrabold)
	  (large
	   :inherit medium
	   :default-height 150)
	  (presentation
	   :default-height 180)
	  (adwaita
	   :default-family "AdwaitaMono Nerd Font Mono"
	   :fixed-pitch-family "AdwaitaMono Nerd Font Mono"
	   :variable-pitch-family "Adwaita Sans")
	  (t
	   ;; I keep all properties for didactic purposes, but most can be
	   ;; omitted.  See the fontaine manual for the technicalities:
	   ;; <https://protesilaos.com/emacs/fontaine>.
	   :default-family "Aporetic Sans Mono"
	   :default-weight regular
	   :default-height 140

         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0

         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Aporetic Serif"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0

         :mode-line-active-family nil ; falls back to :default-family
         :mode-line-active-weight nil ; falls back to :default-weight
         :mode-line-active-height 0.9

         :mode-line-inactive-family nil ; falls back to :default-family
         :mode-line-inactive-weight nil ; falls back to :default-weight
         :mode-line-inactive-height 0.9

         :header-line-family nil ; falls back to :default-family
         :header-line-weight nil ; falls back to :default-weight
         :header-line-height 0.9

         :line-number-family nil ; falls back to :default-family
         :line-number-weight nil ; falls back to :default-weight
         :line-number-height 0.9

         :tab-bar-family nil ; falls back to :default-family
         :tab-bar-weight nil ; falls back to :default-weight
         :tab-bar-height 1.0

         :tab-line-family nil ; falls back to :default-family
         :tab-line-weight nil ; falls back to :default-weight
         :tab-line-height 1.0

         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold

         :italic-family nil
         :italic-slant italic

         :line-spacing nil)))

;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
(fontaine-mode 1))
;; [[https://github.com/protesilaos/fontaine][fontaine]]:1 ends here

;; [[file:emacs.org::*evil][evil:1]]
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
)
;; evil:1 ends here

;; [[file:emacs.org::*evil-collection][evil-collection:1]]
(use-package evil-collection
  :straight t
  :after (evil diminish)
  :diminish (evil-collection-unimpaired-mode)
  :custom
  (evil-collection-setup-minibuffer t) ;; Setup ‘evil’ bindings in the ‘minibuffer’
  (evil-collection-which-key-setup t) ;; Setup ‘evil’ bindings for ‘which-key’.
  :config
  (setq evil-collection-unimpaired-want-repeat-mode-integration t)
  (evil-collection-init))
;; evil-collection:1 ends here

;; [[file:emacs.org::*evil-commentary][evil-commentary:1]]
(use-package evil-commentary
  :straight t
  :after (evil-collection diminish)
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode t))
;; evil-commentary:1 ends here

;; [[file:emacs.org::*evil-surround][evil-surround:1]]
(use-package evil-surround
  :straight t
  :after evil-collection
  :diminish global-evil-surround-mode
  :config
  (global-evil-surround-mode t))
;; evil-surround:1 ends here

;; [[file:emacs.org::*evil-snipe][evil-snipe:1]]
(use-package evil-snipe
  :straight t
  :after (evil diminish)
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
;; evil-snipe:1 ends here

;; [[file:emacs.org::*evil-textobj-tree-sitter][evil-textobj-tree-sitter:1]]
(use-package evil-textobj-tree-sitter
  :straight t
  :after (evil evil-collection)
  :config
  (define-key evil-outer-text-objects-map
	      "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map
	      "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map
	      "c" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map
	      "c" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  )
;; evil-textobj-tree-sitter:1 ends here

;; [[file:emacs.org::*evil-fringe-mark][evil-fringe-mark:1]]
(use-package evil-fringe-mark
  :straight t
  :diminish (global-evil-fringe-mark-mode)
  :config
  (global-evil-fringe-mark-mode))
;; evil-fringe-mark:1 ends here

;; [[file:emacs.org::*general.el][general.el:1]]
(use-package general
  :straight t
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  )
;; general.el:1 ends here

;; [[file:emacs.org::*misc <leader> maps][misc <leader> maps:1]]
(leader-keys
  ;; Execute / Commands
  "<escape>" '(keyboard-escape-quit :which-key t)
  ":" '(execute-extended-command :which-key "execute command")
  "<SPC>" '(popper-toggle :which-key "popper-toggle")
  (kbd "C-SPC") '(popper-toggle :which-key "popper-toggle")
  "<tab>" '(popper-cycle :which-key "popper-cycle"))
;; misc <leader> maps:1 ends here

;; [[file:emacs.org::*<leader> b][<leader> b:1]]
(leader-keys
  "b" '(:ignore t :which-key "Buffer")
  "b <escape>" '(keyboard-escape-quit :which-key t)
  "bk"  '(kill-current-buffer :which-key "Kill Current")
  "bn"  '(next-buffer :which-key "Next")
  "bp"  '(previous-buffer :which-key "Previous")
  "bf"  '(consult-buffer :which-key "Find")
  "bs"  '(scratch-buffer :which-key "Scratch Buffer"))
;; <leader> b:1 ends here

;; [[file:emacs.org::*<leader> e][<leader> e:1]]
(leader-keys
  "e" '(:ignore t :which-key "eglot")
  "e <escape>" '(keyboard-escape-quit :which-key t)
  "ea"  '(eglot-code-actions :which-key "eglot-code-actions"))
;; <leader> e:1 ends here

;; [[file:emacs.org::*<leader> f][<leader> f:1]]
(leader-keys
  "f"  '(:ignore t :which-key "File")
  "f <escape>" '(keyboard-escape-quit :which-key t)
  "fi" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")
  "ff"  '(find-file :which-key "Find")
  "fr"  '(recentf :which-key "Recent")
  "fp"  '(project-find-file :which-key "Project"))
;; <leader> f:1 ends here

;; [[file:emacs.org::*<leader> g][<leader> g:1]]
(leader-keys
  "g" '(:ignore t :which-key "Toggle")
  "g <escape>" '(keyboard-escape-quit :which-key t)
  "gs" '(magit-status :which-key "Status")
  ;; "gn" '(diff-hl-next-hunk :which-key "Next Hunk")
  ;; "gp" '(diff-hl-previous-hunk :which-key "Previous Hunk")
  )
;; <leader> g:1 ends here

;; [[file:emacs.org::*<leader> h][<leader> h:1]]
(leader-keys
  "h" '(:ignore t :which-key "Help")
  "h <escape>" '(keyboard-escape-quit :which-key t)
  "he" '(view-echo-area-messages :which-key "view-echo-area-messages")
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
  "ht" '(my/transient-menu :which-key "Transient")
  "hi" '(consult-info :which-key "Information"))
;; <leader> h:1 ends here

;; [[file:emacs.org::*<leader> l][<leader> l:1]]
(leader-keys
  "l" '(:ignore t :which-key "LLM")
  "l <escape>" '(keyboard-escape-quit :which-key t)
  "ll" '(gptel :which-key "gptel"))
;; <leader> l:1 ends here

(leader-keys
  "n" '(:ignore t :which-key "Note")
  "n <escape>" '(keyboard-escape-quit :which-key t)
  ;; "nj" '(denote-journal-new-or-existing-entry :which-key "journal today")
  ;; "nn" '(denote :which-key "new")
  ;; "nf" '(denote-open-or-create :which-key "find")
  "nf" '(org-roam-node-find :which-key "node find")
  "ni" '(org-roam-node-insert :which-key "node insert")
  "nc" '(org-roam-capture :which-key "capture")
  "ng" '(org-roam-graph :which-key "graph")
  "nl" '(org-roam-buffer-toggle :which-key "toggle")
  "nj" '(org-roam-dailies-capture-today :which-key "capture today")
  "nt" '(org-roam-dailies-goto-today :which-key "today"))

(leader-keys
  "o" '(:ignore t :which-key "Open")
  "o <escape>" '(keyboard-escape-quit :which-key t)
  "oe" '(eshell :which-key "eshell")
  "oo" '(:ignore t :which-key "Other Window"))

;; [[file:emacs.org::*<leader> p][<leader> p:1]]
(leader-keys
   ;; Project
  "p" '(:ignore t :which-key "Project")
  "p <escape>" '(keyboard-escape-quit :which-key t)
  "pf" '(project-find-file :which-key t))
;; <leader> p:1 ends here

;; [[file:emacs.org::*<leader> r][<leader> r:1]]
(leader-keys
  ;; Bookmark / Recent
  "r" '(:ignore t :which-key "Recent")
  "r <escape>" '(keyboard-escape-quit :which-key t)
  "rb" '(my/revert-buffer-from-file :which-key "kill & re-open buffer")
  "rj" '(bookmark-jump :which-key "bookmark-jump")
  "rm" '(bookmark-set :which-key "bookmark-set")
  "rl" '(bookmark-bmenu-list :which-key "bookmark-bmenu-list")
  "ru" '(vundo :which-key "Undo tree")
  "rf" '(consult-bookmark :which-key "find bookmark")
  "rd" '(bookmark-delete :which-key "bookmark delete"))
;; <leader> r:1 ends here

;; [[file:emacs.org::*<leader> s][<leader> s:1]]
(leader-keys
;;;; general s
  "s" '(:ignore t :which-key "Search")
  "s <escape>" '(keyboard-escape-quit :which-key t)
  "sr" '(consult-ripgrep :which-key "ripgrep")
  "so" '(consult-outline :which-key "outline")
  "sh" '(consult-org-heading :which-key "heading")
  "sl" '(consult-line :which-key "line")
  "su" '(vundo :which-key t "undo")
  "st" '(consult-todo-project :which-key t "todo")
  "sG" '(consult-git-grep :which-key t "git-grep"))
;; <leader> s:1 ends here

;; [[file:emacs.org::*<leader> t][<leader> t:1]]
(leader-keys
  "t" '(:ignore t :which-key "Toggle")
  "t <escape>" '(keyboard-escape-quit :which-key t)
  "tt" '(modus-themes-toggle :which-key "Theme")
  "tr" '(rainbow-mode  :which-key "Rainbow")
  "tl" '(toggle-truncate-lines :which-key "truncate lines")
  "tp" '(popper-toggle :which-key "popper-toggle")
  "tk" '(keycast-mode-line-mode :which-key "keycast mode-line"))
;; <leader> t:1 ends here

;; [[file:emacs.org::*<leader> u][<leader> u:1]]
(leader-keys
"u" '(universal-argument :which-key "universal argument"))
;; <leader> u:1 ends here

;; [[file:emacs.org::*<leader> w][<leader> w:1]]
(leader-keys
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
  "wgt" '(evil-tab-next :which-key "next tab")
  "wgT" '(tab-bar-switch-to-prev-tab :which-key "previous tab")
  "wu" '(winner-undo :which-key "undo")
  )
;; <leader> w:1 ends here

;; [[file:emacs.org::*Repeat keys][Repeat keys:1]]
(use-package emacs
  :config
  (repeat-mode 1))
;; Repeat keys:1 ends here

;; [[file:emacs.org::*expreg][expreg:1]]
(use-package expreg
  :straight t
  :bind (("C-+" . expreg-expand)
	 ("C--" . expreg-contract)))
;; expreg:1 ends here

;; [[file:emacs.org::*Icelandic vim keys][Icelandic vim keys:1]]
(use-package emacs
  :after (evil)
  :config
  (evil-global-set-key 'normal "þ" 'evil-search-forward)
  (evil-global-set-key 'normal "Þ" 'evil-search-backward))
;; Icelandic vim keys:1 ends here

;; [[file:emacs.org::*tree-sitter-langs][tree-sitter-langs:1]]
(use-package tree-sitter-langs
  :straight t
  :config
  (setq treesit-load-name-override-list
	  '((python "python" "tree_sitter_python")
	    (nix "nix" "tree_sitter_nix")
	    (json "json" "tree_sitter_json")
	    (yaml "yaml" "tree_sitter_yaml")
	    (elisp "elisp" "tree_sitter_elisp")
	    (markdown "markdown" "tree_sitter_markdown")
	    (markdown-inline "markdown-inline" "tree_sitter_markdown_inline")
	    (julia "julia" "tree_sitter_julia")
	    ))
  (setq treesit-extra-load-path
	  (list tree-sitter-langs--dir
		(concat tree-sitter-langs--dir "bin/")))
  )
;; tree-sitter-langs:1 ends here

;; [[file:emacs.org::*treesit-fold][treesit-fold:1]]
(use-package treesit-fold
  :straight t
  :config
  (setq treesit-fold-line-count-show t
	treesit-fold-line-count-format " <%d lines> ")

  ;; Add support for non-ts modes
  ;; (add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp)))
  (global-treesit-fold-mode))
;; treesit-fold:1 ends here

;; [[file:emacs.org::*org-roam][org-roam:1]]
(use-package org-roam
  :straight t
  :after (org)
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
;; org-roam:1 ends here

;; [[file:emacs.org::*gcmh][gcmh:1]]
(use-package gcmh
  :straight t
  :after (diminish)
  :diminish gcmh-mode
  :hook
  (after-init-hook . gcmh-mode))
;; gcmh:1 ends here

;; [[file:emacs.org::*/[/[https:/github.com/jdtsmith/ultra-scroll/]/[ultra-scroll/]/]][[[https://github.com/jdtsmith/ultra-scroll][ultra-scroll]]:1]]
(use-package ultra-scroll
  :straight (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
	  scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))
;; [[https://github.com/jdtsmith/ultra-scroll][ultra-scroll]]:1 ends here

;; [[file:emacs.org::*org][org:1]]
(use-package org
  :config
  (setq org-blank-before-new-entry
	'((heading . always)
	    (plain-list-item . auto)))
  (setq org-auto-align-tags nil
	org-hide-emphasis-markers t
	org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE")))
  ;; (add-hook 'org-mode-hook #'my/org-mode-entry)
  ;; Adjusts the scaling of latex previews, perhaps only needed on Windows?  
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (emacs-lisp t)
     (shell . t)
     (julia . t)
     ))
  ;; (add-to-list 'org-src-lang-modes '("python" . python-ts))
  ;; (add-to-list 'org-src-lang-modes '("python" . python))
  (add-hook 'org-mode-hook #'my/org-auto-tangle-enable)
  (add-hook 'org-mode-hook #'my/org-mode-entry)
  )
;; org:1 ends here

;; [[file:emacs.org::*org-mouse][org-mouse:1]]
(use-package org-mouse
  :after (org)
  :config
  (require 'org-mouse))
;; org-mouse:1 ends here

;; [[file:emacs.org::*org-modern][org-modern:1]]
(use-package org-modern
  :straight t
  :after (org)
  :config
  (setq org-modern-table nil
	  org-modern-block-name nil
	  org-modern-block-fringe nil
	  org-modern-star 'replace
        ;; org-modern-replace-stars "◉○◈◇✳"
        org-modern-replace-stars "§◉○◈◇"))
;; org-modern:1 ends here

;; [[file:emacs.org::*htmlize][htmlize:1]]
(use-package htmlize
  :straight t)
;; htmlize:1 ends here

;; [[file:emacs.org::*Emacs Speaks Statistics][Emacs Speaks Statistics:1]]
(use-package ess
  :straight t
  :defer t)
;; Emacs Speaks Statistics:1 ends here

;; [[file:emacs.org::*elisp][elisp:1]]
(use-package emacs
  :config
  ;; Enable the use of outline-mode when editing elisp files.
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))
;; elisp:1 ends here

;; [[file:emacs.org::*jula-mode][jula-mode:1]]
(use-package julia-mode
  :straight t
  :defer t)
;; jula-mode:1 ends here

;; [[file:emacs.org::*julia-ts-mode][julia-ts-mode:1]]
(use-package julia-ts-mode
  :straight t
  :mode "\\.jl$"
  :init
  ;; (add-to-list 'org-babel-load-languages '(julia . t))
  ;; (org-babel-do-load-languages 'org-babel-load-languages)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((julia . t)))
  )
;; julia-ts-mode:1 ends here

;; [[file:emacs.org::*auctex][auctex:1]]
(use-package auctex
  :straight t
  :defer t
  :config
  (setq TeX-auto-save t
	TeX-parse-self t)
  )
;; auctex:1 ends here

;; [[file:emacs.org::*nix-mode][nix-mode:1]]
(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'")
;; nix-mode:1 ends here

;; [[file:emacs.org::*nix-ts-mode][nix-ts-mode:1]]
(use-package nix-ts-mode
  :straight t
  :if (treesit-language-available-p 'nix)
  :defer t
  :init
  (setq major-mode-remap-alist
	(append major-mode-remap-alist
		'((nix-mode . nix-ts-mode)))))
;; nix-ts-mode:1 ends here

;; [[file:emacs.org::*conf-mode][conf-mode:1]]
(use-package conf-mode
  :straight nil ;; builtin
  :mode "\\.inputrc\\'"
  :hook
  (conf-mode . display-line-numbers-mode)
  (conf-mode . (lambda () (setq-local truncate-lines t)))         ;; Enable line numbers 
  )
;; conf-mode:1 ends here

;; [[file:emacs.org::*yaml][yaml:1]]
(use-package yaml-ts-mode
  :if (treesit-language-available-p 'yaml)
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-ts-mode))
  :hook ((yaml-ts-mode . (lambda () (setq-local tab-width 2))))
  )
;; yaml:1 ends here

;; [[file:emacs.org::*python][python:1]]
(use-package python
  :init
  (if (treesit-language-available-p 'python)
	;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
    (setq major-mode-remap-alist
	    (append major-mode-remap-alist
		  '((python-mode . python-ts-mode))))
    ))
;; python:1 ends here

;; [[file:emacs.org::*eglot][eglot:1]]
(use-package eglot
  :init
  (add-hook 'nix-mode-hook 'eglot-ensure)
  (add-hook 'nix-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  :defer t)
;; eglot:1 ends here

;; [[file:emacs.org::*format-all][format-all:1]]
(use-package format-all
  :disabled
  :straight t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
		  '(("C"     (astyle "--mode=c"))
		    ("Shell" (shfmt "-i" "4" "-ci"))
		    ;; ("Nix" (alejandra "--quiet"))
		    ))
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter))
;; format-all:1 ends here

;; [[file:emacs.org::*rainbow-mode][rainbow-mode:1]]
(use-package rainbow-mode
  :straight t)
;; rainbow-mode:1 ends here

;; [[file:emacs.org::*qrencode][qrencode:1]]
(use-package qrencode
  :straight t)
;; qrencode:1 ends here

;; [[file:emacs.org::*keycast][keycast:1]]
(use-package keycast
  :straight t)
;; keycast:1 ends here

;; [[file:emacs.org::*vterm][vterm:1]]
(condition-case nil
    (progn
	(require 'vterm)
	(use-package vterm
	:after (general)
	:config
	(leader-keys
	  "ot" '(vterm :which-key "vterm")
	  "oot" '(vterm-other-window :which-key "vterm"))
	;; Make vterm more responsive
	(setq vterm-timer-delay 0.01)
	(setq vterm-kill-buffer-on-exit t)))
  (error
   (message "vterm not installed")))
;; vterm:1 ends here

;; [[file:emacs.org::*jinx][jinx:1]]
(condition-case nil
    (progn
      (require 'jinx)
      (use-package jinx
	:after (general)
	:config
	(leader-keys
	  "ts" '(jinx-mode :which-key "jinx"))))
  (error
   (message "jinx not installed")))
;; jinx:1 ends here

;; [[file:emacs.org::*scratch][scratch:1]]
(use-package scratch
  :straight t
  :config

  (add-hook 'org-mode-hook
            (lambda ()
              (when scratch-buffer
                (save-excursion
                  (goto-char (point-min))
                  (insert "#+TITLE: Scratch Buffer\n\n")))))
  (leader-keys
    "os" '((lambda () (interactive)
	    (let ((current-prefix-arg '(4))) ; Equivalent to C-u
	      (call-interactively 'scratch))) :which-key "scratch")))
;; scratch:1 ends here

;; [[file:emacs.org::*envrc][envrc:1]]
(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))
;; envrc:1 ends here

;; [[file:emacs.org::*evil-terminal-cursor-changer][evil-terminal-cursor-changer:1]]
(use-package evil-terminal-cursor-changer
  :straight t
  :if (not (display-graphic-p))
  :config
  (evil-terminal-cursor-changer-activate))
;; evil-terminal-cursor-changer:1 ends here

;; [[file:emacs.org::*pulsar][pulsar:1]]
(use-package pulsar
  :disabled
  :straight t
  :config
  ;; (add-to-list 'pulsar-pulse-region-functions 'evil-yank)
  ;; (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-backward)
  (setq pulsar-pulse-functions (remove 'evil-scroll-up pulsar-pulse-functions))
  (setq pulsar-pulse-functions (remove 'evil-scroll-down pulsar-pulse-functions))
  (pulsar-global-mode))
;; pulsar:1 ends here

;; [[file:emacs.org::*Casual][Casual:1]]
(use-package casual
  :straight t)
;; Casual:1 ends here

;; [[file:emacs.org::*spacious-padding][spacious-padding:1]]
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
;; spacious-padding:1 ends here

;; [[file:emacs.org::*vi-tilde-fringe][vi-tilde-fringe:1]]
(use-package vi-tilde-fringe
  :straight t
  :diminish vi-tilde-fringe-mode
  :hook (prog-mode-hook . vi-tilde-fringe-mode))
;; vi-tilde-fringe:1 ends here

;; [[file:emacs.org::*Show current project][Show current project:1]]
(use-package project
  :custom
  (project-mode-line t))
;; Show current project:1 ends here

;; [[file:emacs.org::*Hide line/column numbers][Hide line/column numbers:1]]
(use-package emacs
  :init
  (line-number-mode -1)
  (column-number-mode -1))
;; Hide line/column numbers:1 ends here

;; [[file:emacs.org::*diminish][diminish:1]]
(use-package diminish
  :straight t
  :config
  (add-hook 'after-init-hook
  (lambda ()
  ;; Your custom initialization code here
  (message "Diminish-ing modes")
  (diminish 'buffer-face-mode)
  (diminish 'auto-revert-mode)
  (diminish 'visual-line-mode)
  (diminish 'treesit-fold-mode)
  (diminish 'global-evil-fringe-mark-mode)
  ;; (diminish 'buffer-face-mode)
  ;; (diminish 'BufFace)
  ))  
)
;; diminish:1 ends here

;; [[file:emacs.org::*nerd-icons][nerd-icons:1]]
(use-package nerd-icons
  :straight t)
;; nerd-icons:1 ends here

;; [[file:emacs.org::*nerd-icons-dired][nerd-icons-dired:1]]
(use-package nerd-icons-dired
  :straight t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))
;; nerd-icons-dired:1 ends here

;; [[file:emacs.org::*nerd-icons-completion][nerd-icons-completion:1]]
(use-package nerd-icons-completion
  :straight t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )
;; nerd-icons-completion:1 ends here

;; [[file:emacs.org::*nerd-icons-corfu][nerd-icons-corfu:1]]
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
;; nerd-icons-corfu:1 ends here

;; [[file:emacs.org::*Mouse Support][Mouse Support:1]]
;; right click mouse menu
(context-menu-mode 1)
;; Mouse Support:1 ends here

;; [[file:emacs.org::*Theme][Theme:1]]
;; Mark all themes as safe so emacs won't ask & annoy you
(setq custom-safe-themes t)
;; Theme:1 ends here

;; [[file:emacs.org::*modus-themes][modus-themes:1]]
(use-package modus-themes
  :straight t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
  	modus-themes-bold-constructs t)

  (setq modus-themes-common-palette-overrides
	`((fg-region unspecified)
	  (bg-region bg-sage)
	  
	  ;; A nuanced accented background, combined with a suitable foreground.
	  (bg-prose-code bg-green-nuanced)
	  (fg-prose-code green-cooler)
	  
	  (bg-prose-verbatim bg-magenta-nuanced)
	  (fg-prose-verbatim magenta-warmer)
	  
	  (bg-prose-macro bg-blue-nuanced)
	  (fg-prose-macro magenta-cooler)))
  
  (setq modus-vivendi-palette-overrides
	`((bg-main "#000000")
	  (fg-heading-1 magenta-faint)
	  (bg-line-number-active "#2f3849")))
  
  (setq modus-operandi-palette-overrides
	'((fg-heading-1 "#2f5f9f")
	  (bg-region "#c0deff")
	  (bg-line-number-active "#dae5ec")))

  (setq modus-themes-headings
  	'((0 . (1.35))
          (1 . (1.30))
          (2 . (1.24))
          (3 . (semibold 1.17))
          (4 . (1.14))
          (t . (monochrome))))

  (add-hook 'modus-themes-after-load-theme-hook #'my/modus-themes-custom-faces)
  (add-hook 'after-init-hook #'my/modus-themes-custom-faces)
  (load-theme 'modus-vivendi))
;; modus-themes:1 ends here

;; [[file:emacs.org::*auto-dark][auto-dark:1]]
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
;; auto-dark:1 ends here

;; [[file:emacs.org::*visual-fill-column][visual-fill-column:1]]
(use-package visual-fill-column
  ;; :disabled t
  :straight t
  :defer t
  :custom
  (visual-fill-column-width 100)
  :config  
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))
  :hook (org-mode . (lambda ()
			(visual-fill-column-mode)
			;; (visual-line-fill-column-mode)
			(visual-fill-column-toggle-center-text)))
  )
;; visual-fill-column:1 ends here

;; [[file:emacs.org::*Disable scrollbars in the minibuffer][Disable scrollbars in the minibuffer:1]]
(use-package emacs
  :config
  (set-window-scroll-bars (minibuffer-window) nil nil nil nil 1)
  (set-window-parameter (get-buffer-window "*Messages*") 'vertical-scroll-bars nil))
;; Disable scrollbars in the minibuffer:1 ends here

;; [[file:emacs.org::*Fullscreen][Fullscreen:2]]
(use-package emacs
  :config
  (add-hook 'after-make-frame-functions
	      (lambda (frame) 
			(with-selected-frame frame
		  (my/toggle-menu-bar-in-fullscreen))))

  (add-hook 'window-configuration-change-hook
	    'my/toggle-menu-bar-in-fullscreen))
;; Fullscreen:2 ends here

;; [[file:emacs.org::*gptel][gptel:1]]
(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode #'org-mode))
;; gptel:1 ends here

;; [[file:emacs.org::*Follow symlinks][Follow symlinks:1]]
(use-package emacs
  :config
  ;; Always follow symbolic link to a file under version control.
  (setq vc-follow-symlinks t))
;; Follow symlinks:1 ends here

;; [[file:emacs.org::*magit][magit:1]]
(use-package magit
  :straight t)
;; magit:1 ends here

;; [[file:emacs.org::*Handled backends][Handled backends:1]]
(use-package vc
  :config
  (setq vc-handled-backends '(Git)))
;; Handled backends:1 ends here

;; [[file:emacs.org::*diff-hl][diff-hl:1]]
(use-package diff-hl
  :straight t
  :after (magit)
  :config
  (global-diff-hl-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Re-center the window around curser after jumping hunks
  (advice-add 'diff-hl-next-hunk :after (lambda (&rest _args) (recenter)))
  (advice-add 'diff-hl-previous-hunk :after (lambda (&rest _args) (recenter)))
  )
;; diff-hl:1 ends here

;; [[file:emacs.org::*helpful][helpful:1]]
(use-package helpful
  :straight t
:config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))
;; helpful:1 ends here

;; [[file:emacs.org::*which-key][which-key:1]]
(use-package which-key
  :after (diminish)
  :diminish (which-key-mode)
  :config
  (which-key-mode 1))
;; which-key:1 ends here

;; [[file:emacs.org::*eldoc][eldoc:1]]
(use-package eldoc
  :after (diminish)
  :diminish eldoc-mode)
;; eldoc:1 ends here

;; [[file:emacs.org::*popper][popper:1]]
(defun my/max-window-height ()
  "Return the maximum of the output of `popper--fit-window-height` and 10."
  (max (popper--fit-window-height) 10))
;; popper:1 ends here

;; [[file:emacs.org::*popper][popper:2]]
(use-package popper
  :straight t
  ;; :after (setup-windows setup-project)
  :commands popper-mode
  :init
  (if (boundp 'elpaca-after-init-hook)
	(add-hook 'elpaca-after-init-hook #'popper-mode)
    (add-hook 'emacs-startup-hook #'popper-mode))
  (setq popper-window-height 'my/max-window-height)
  ;; (setq popper-display-function #'display-buffer-pop-up-window)
  ;; (setq popper-display-function #'display-buffer-in-child-frame)
  (setq popper-reference-buffers
	  (append my/help-modes-list
		  ;; Match eshell, shell, term and/or vterm buffers
		  '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
		    "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
		    "^\\*term.*\\*$"   term-mode   ;term as a popup
		    "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
		    )))
  )
;; popper:2 ends here

;; [[file:emacs.org::*Layout History][Layout History:1]]
(use-package emacs
  :init
  (winner-mode 1))
;; Layout History:1 ends here

;; [[file:emacs.org::*Toggle org-mode emphasis markers][Toggle org-mode emphasis markers:2]]
(define-key org-mode-map (kbd "C-c e") #'my/org-toggle-hide-emphasis-markers)
;; Toggle org-mode emphasis markers:2 ends here
