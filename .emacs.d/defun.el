;;; -*- lexical-binding: t -*-

;; [[file:emacs.org::*<leader> r][<leader> r:2]]
(defun my/revert-buffer-from-file ()
"Kill and reopen the current buffer from its associated file."
(interactive)
(let ((file (buffer-file-name)))
  (if file
	(progn
	  (kill-buffer (current-buffer))
	  (find-file file))
    (message "Current buffer is not visiting a file."))))
;; <leader> r:2 ends here

;; [[file:emacs.org::*org-modern][org-modern:2]]
(defun my/org-mode-entry ()
    "Enable visual line mode and set word wrap in non-programming modes."
    (org-modern-mode 1))
;; org-modern:2 ends here

;; [[file:emacs.org::*Programming Specific Editor Configuration][Programming Specific Editor Configuration:1]]

;; Programming Specific Editor Configuration:1 ends here

;; [[file:emacs.org::*dired][dired:1]]
(defun my/transient-menu ()
  "Toggle transient menus based on derived modes."
  (interactive)
  (when (derived-mode-p 'dired-mode)
    (casual-dired-tmenu))
  )
;; dired:1 ends here

;; [[file:emacs.org::*modus-themes][modus-themes:2]]
(defun my/modus-themes-custom-faces (&rest _)
(interactive)  
(modus-themes-with-colors
(custom-set-faces

 ;; Change nerd-icons folder colors
 '(nerd-icons-folder ((t (:foreground "#008899"))))
 '(nerd-icons-folder-open ((t (:foreground "#008899"))))
 '(nerd-icons-completion-dir-face ((t (:foreground "#008899"))))
 '(nerd-icons-dired-dir-face ((t (:foreground "#008899"))))


 ;; diff-hl fringe/margin colors
 '(diff-hl-insert ((t (:background "#88ca9f" :foreground "#092f1f"))))
 '(diff-hl-delete ((t (:background "#ff7f86" :foreground "#3a0c14"))))
 '(diff-hl-change ((t (:background "#dfaf7a" :foreground "#381d0f"))))

 ;; org-mode mixed fonts
 '(org-checkbox ((t (:inherit 'fixed-pitch))))
 '(org-block ((t (:foreground nil :inherit 'fixed-pitch))))
 '(org-block-begin-line ((t (:inherit 'fixed-pitch))))
 ;; '(org-block-end-line ((t (:inherit 'fixed-pitch))))
 '(org-table ((t (:inherit 'fixed-pitch))))
 '(org-code ((t (:inherit 'fixed-pitch))))
 ;; '(org-verbatim ((t (:inherit 'fixed-pitch))))

 ;; corfu mixed fonts
 '(corfu-default ((t (:inherit 'fixed-pitch))))
 
 ;; Vundo symbol colors
 '(vundo-saved ((t (:foreground "#008899"))))
 '(vundo-last-saved ((t (:foreground "#900276"))))

 ;; fringe-indicators
 '(fringe ((t (:foreground "#008899"))))
 )))
;; modus-themes:2 ends here

;; [[file:emacs.org::*Fullscreen][Fullscreen:1]]
(defun my/toggle-menu-bar-in-fullscreen ()
  "Toggle `menu-bar-mode' when entering or exiting fullscreen."
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
	(menu-bar-mode -1)  ; Turn off menu bar in fullscreen
    (menu-bar-mode 1)))   ; Turn on menu bar when not in fullscreen
;; Fullscreen:1 ends here

;; [[file:emacs.org::*Auto tangle on save][Auto tangle on save:1]]
(defun my/org-auto-tangle ()
  "Automatically tangle Org file on save, but only if the file contains '#+auto_tangle: t'."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^#\\+auto_tangle: t" nil t)))
    (org-babel-tangle)))

(defun my/org-auto-tangle-enable ()
  "Enable auto-tangling for this buffer."
  (add-hook 'after-save-hook #'my/org-auto-tangle nil 'local))
;; Auto tangle on save:1 ends here

;; [[file:emacs.org::*Toggle org-mode emphasis markers][Toggle org-mode emphasis markers:1]]
(defun my/org-toggle-hide-emphasis-markers ()
  "Toggle `org-hide-emphasis-markers' locally and refresh fontification."
  (interactive)
  (if (bound-and-true-p org-hide-emphasis-markers)
      (setq-local org-hide-emphasis-markers nil)
    (setq-local org-hide-emphasis-markers t))
  ;; Refresh fontification
  (font-lock-flush)
  (font-lock-ensure)
  ;; (message "org-hide-emphasis-markers is now %s" org-hide-emphasis-markers)
  )
;; Toggle org-mode emphasis markers:1 ends here
