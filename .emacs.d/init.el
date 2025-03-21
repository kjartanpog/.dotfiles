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
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  
  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.
  
  :config
  (context-menu-mode 1)                           ;; Enable right click mouse menu.
  (cua-mode 1)                                    ;; use C-z, C-x, C-c, and C-v to undo, cut, copy, and paste
  (recentf-mode t)                                ;; Enable tracking of recently opened files.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p (concat user-emacs-directory "custom.el"))
    (load (concat user-emacs-directory "custom.el")))
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/"))))
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups/") t)))
  
  :init                                           ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)                              ;; Disable the tool bar for a cleaner interface.
  )

;; Bookmarks, History & Undo

;;(use-package bookmark
;;  :config
;;  (setopt bookmark-save-flag 1)
;;  (run-at-time nil (* 5 60) #'bookmark-save))

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


;; Keymaps

(use-package which-key
  :config
  (which-key-mode t))

;; Fonts & Theme

(use-package modus-themes
  :straight t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  ;;(setq modus-themes-common-palette-overrides
  ;;      modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm))
