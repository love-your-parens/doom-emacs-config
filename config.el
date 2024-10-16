;;; shared-config.el -*- lexical-binding: t; -*-

;;; Workstation-agnostic, version-controlled, universal settings.


;; Here are some settings specific to workstation and environment,
;; which you'll want to put in the main configuration file instead.
;;
;; Initial frame size & position:
;; (add-to-list 'initial-frame-alist '(width . 120))
;; (add-to-list 'initial-frame-alist '(height . 40))
;; (add-to-list 'initial-frame-alist '(left . 0.5))
;; (add-to-list 'initial-frame-alist '(top . 0.5))
;;
;; Subsequent frames:
;; (add-to-list 'default-frame-alist '(width . 120))
;; (add-to-list 'default-frame-alist '(height . 40))
;;
;; Splash ASCII-art replacement:
;; (setq fancy-splash-image (concat doom-user-dir "splash/emacs.svg"))


;;; General settings
(setq display-line-numbers-type 'relative)
(after! treemacs (treemacs-follow-mode 1))
(setq calendar-week-start-day 1) ; weeks start on mondays

;; LSP
(setenv "LSP_USE_PLISTS" "true") ; performance
(setq lsp-headerline-breadcrumb-enable t
      lsp-file-watch-threshold nil)


;;; Editor functionality
(defun center-frame (&optional frame)
  "Puts the selected frame in the middle of the screen."
  (interactive)
  (when window-system
    (modify-frame-parameters frame
                             '((left . 0.5)
                               (top . 0.5)))))

(let ((decorated t))
  (defun toggle-frame-decoration (&optional frame)
    "Toggles frame decoration on/off."
    (interactive)
    (modify-frame-parameters frame
                             (cons (cons 'undecorated decorated)
                                   '((left . 0.5)
                                     (top . 0.5))))
    (setq decorated (not decorated))))

(defun set-default-frame-size (&optional frame)
  "Sets the frame size to default dimensions.
   Acts only if both default width and height have been configured."
  (interactive)
  (when window-system
    (when-let ((width (cdr (assq 'width default-frame-alist))))
      (when-let ((height (cdr (assq 'height default-frame-alist))))
        (set-frame-size frame width height)))))

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
(map! "C-M-," #'sp-forward-barf-sexp)
(map! "C-M-." #'sp-forward-slurp-sexp)
(map! "S-M-<up>" #'sp-raise-sexp)
(map! "S-M-<down>" #'sp-splice-sexp)
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

(map! :map clojure-mode-map
      :leader
      "c f" #'lsp-format-buffer
      "c F" #'lsp-format-region)


;; PHP
(map! :after php-mode
      :map php-mode-map
      :leader
      "c f" #'lsp-format-buffer
      "c F" #'lsp-format-region)


;;; Markdown
;; Bring the source markup view closer in appearance to the end-result.
;; Differentiate the headings, add a little bit of line spacing.
(add-hook! markdown-mode
  (let ((family (symbol-name (font-get doom-serif-font :family))))
    (custom-set-faces!
      `(markdown-header-face-1  :height 2.25 :family ,family :weight black :inherit markdown-header-face)
      `(markdown-header-face-2  :height 1.50 :family ,family :inherit markdown-header-face)
      `(markdown-header-face-3  :height 1.25 :family ,family :inherit markdown-header-face)
      `(markdown-header-face-4  :height 1.125 :family ,family :inherit markdown-header-face)
      `(markdown-header-face-5  :height 1.0 :family ,family :inherit markdown-header-face)
      `(markdown-header-face-6  :height 1.0 :family ,family :weight semibold :inherit markdown-header-face)))
  (setq writeroom-extra-line-spacing 1))
