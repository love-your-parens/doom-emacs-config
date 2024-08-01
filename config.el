(setq doom-font (font-spec :family "Iosevka Nerd Font"
                           :size 16
                           :weight 'regular)
      doom-big-font (font-spec :family "Iosevka Nerd Font" :size 24)
      doom-serif-font (font-spec :family "Iosevka Slab")
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" 
                                          :weight 'regular))


;;; General settings
(after! treemacs (treemacs-follow-mode 1))

;; Display
(setq default-frame-width 150)
(setq default-frame-height 60)

;; LSP
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-headerline-breadcrumb-enable t
      lsp-intelephense-licence-key ""
      lsp-file-watch-threshold nil)


;;; Editor functionality
(defun set-default-frame-size-and-position (&optional frame)
  (set-frame-size frame
                  default-frame-width
                  default-frame-height)
  (modify-frame-parameters frame
                           '((left . 0.5)
                             (top . 0.5))))

(let ((decorated t))
  (defun toggle-frame-decoration (&optional frame)
    "Toggles frame decoration on/off."
    (interactive)
    (modify-frame-parameters frame
                             (cons (cons 'undecorated decorated)
                                   '((left . 0.5)
                                     (top . 0.5))))
    (setq decorated (not decorated))))

(defun window-next-enlargen ()
  "Switches to the next window and enlarges it."
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

(defun backward-same-syntax (arg)
  (interactive "^p")
  (forward-same-syntax (- (or arg 1))))


;;; Keybindings
;; Editor
(map! :leader :desc "Toggle frame decoration" "t d" #'toggle-frame-decoration)
(map! :nvi "M-<left>" #'backward-same-syntax)
(map! :nvi "M-<right>" #'forward-same-syntax)
(map! :leader "w O" #'window-next-enlargen)
(map! "M-<backspace>" #'evil-delete-backward-word)
(map! "C-\"" #'paredit-doublequote)


;; Structural editing
(map! "S-<down>" #'sp-up-sexp)
(map! "S-<up>" #'sp-backward-down-sexp)
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


;;; Frame setup.
(when window-system
  (set-default-frame-size-and-position)
  (add-hook 'after-make-frame-functions
            #'set-default-frame-size-and-position))
