(setq doom-font (font-spec :family "Iosevka Nerd Font"
                           :size 16
                           :weight 'regular)
      doom-big-font (font-spec :family "Iosevka Nerd Font" :size 24)
      doom-serif-font (font-spec :family "Iosevka Slab")
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" 
                                          :weight 'regular))

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

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

;; DAP Debugging
(map! :leader
      (:prefix ("d" . "debugging")
       :desc "Toggle breakpoint" "t" #'dap-breakpoint-toggle
       :desc "Start debugging" "d" #'dap-debug
       :desc "Stop debugging" "c" #'dap-disconnect))

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

(add-hook 'php-mode-hook 'lsp-headerline-breadcrumb-mode)

;; LSP
;;; INTELEPHENSE license key
(setq lsp-intelephense-licence-key "")
