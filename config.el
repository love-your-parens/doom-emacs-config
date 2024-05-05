(setq doom-font (font-spec :size 15))

;; Resize & reposition the window in GUI mode:
(when window-system
 (set-frame-size (selected-frame) 120 60)
 (modify-frame-parameters (selected-frame) '((top . 0.5) (left . 0.5))))

;; Doom splash screen logo
;; (setq fancy-splash-image (concat doom-user-dir "splash/doomEmacs.svg"))

;; Editor functionality
(defun window-next-enlargen ()
  (interactive)
  (evil-window-next nil)
  (doom/window-enlargen)
  (evil-scroll-left 2))

;;; Keybindings
;; Editor
(map! :leader "w O" #'window-next-enlargen)
(map! "M-<left>" #'evil-backward-word-end)
(map! "M-<right>" #'evil-forward-word-begin)
(map! "M-<backspace>" #'evil-delete-backward-word)

;; Structural editing
(map! "S-<down>" #'sp-up-sexp)
(map! "S-<up>" #'sp-backward-up-sexp)
(map! "C-M-," #'sp-backward-slurp-sexp)
(map! "C-M-." #'sp-forward-slurp-sexp)
(map! "M-9" #'sp-wrap-round)
(map! "M-[" #'sp-wrap-square)
(map! "M-{" #'sp-wrap-curly)

;; CIDER
(map! :after cider
      :map clojure-mode-map
      "<backtab>" #'cider-format-defun
      "C-M-<return>" #'cider-eval-sexp-at-point)

(map! :after cider
      :map clojure-mode-map
      :localleader
      "e s" #'cider-eval-sexp-at-point)

;; PHP
(map! :after php-mode
      :map php-mode-map
      :leader
      "c f" #'lsp-format-buffer
      "c F" #'lsp-format-region)

;; LSP
;;; INTELEPHENSE license key
(setq lsp-intelephense-licence-key "")
