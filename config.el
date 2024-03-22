(setq doom-font (font-spec :size 15))

;; Resize & reposition the window in GUI mode:
(when window-system
 (set-frame-size (selected-frame) 120 60)
 (modify-frame-parameters (selected-frame) '((top . 0.5) (left . 0.5))))

;; Doom splash screen logo
;; (setq fancy-splash-image (concat doom-user-dir "splash/doomEmacs.svg"))

;; Keybindings
;;; Paredit
(global-set-key (kbd "S-<down>") #'sp-up-sexp)
(global-set-key (kbd "S-<up>") #'sp-backward-up-sexp)
(global-set-key (kbd "S-<right>") #'sp-forward-slurp-sexp)
(global-set-key (kbd "S-<left>") #'sp-backward-slurp-sexp)
(global-set-key (kbd "C-M-,") #'sp-backward-barf-sexp)
(global-set-key (kbd "C-M-.") #'sp-forward-barf-sexp)
