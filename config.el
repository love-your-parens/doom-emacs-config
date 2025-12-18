;;; config.el -*- lexical-binding: t; -*-

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

(setq calendar-week-start-day 1 ; weeks start on mondays
      evil-move-beyond-eol t ; vim-like behaviour not helpful, causes a ton of hiccups
      tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=/tmp/tramp.%%C -o ControlPersist=yes" ; use persistent SSH connections
      which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.01
      lsp-modeline-code-action-fallback-icon  "󱉖"
      display-line-numbers-type 'relative
      cider-enrich-classpath t
      +zen-text-scale 1.1)
(after! treemacs (treemacs-follow-mode 1))

;; MacOS-specific settings.
(when (featurep :system 'macos)
  (setq consult-locate-args "mdfind" ; supplants /bin/locate - preferred in OSX
        lsp-file-watch-threshold 10000 ; guards against exhausting the open-file limit in OSX
        ))

;; Use absolute line numbers in insert mode, relative elsewhere.
(add-hook! 'evil-insert-state-entry-hook
  (when display-line-numbers (setq display-line-numbers t)))
(add-hook! 'evil-insert-state-exit-hook
  (when display-line-numbers (setq display-line-numbers 'relative)))
;; Disable line numbers in certain modes.
(add-hook! 'vterm-mode-hook (setq display-line-numbers nil))

;; Dragging lines and regions with M-up/down.
(drag-stuff-global-mode t)
(drag-stuff-define-keys)

;; Enable CIDER completions and definitions even when LSP is active.
(add-hook 'cider-mode-hook (lambda () (add-to-list 'completion-at-point-functions 'cider-complete-at-point)))
(add-hook 'cider-connected-hook (lambda () (set-lookup-handlers! '(cider-mode cider-repl-mode)
                                             :definition #'+clojure-cider-lookup-definition
                                             :documentation #'cider-doc)))

;; CIDER REPL-popup should have its own modeline. This is to display the progress indicator.
(set-popup-rule! "^\\*cider-repl"
  :size 0.2
  ;; Only the essentials.
  :modeline '((:eval (doom-modeline-segment--bar))
              (:eval (doom-modeline-segment--window-state))
              (:eval (doom-modeline-segment--modals))
              (:eval (doom-modeline-segment--major-mode))
              (:eval (doom-modeline-segment--process))))
(setq cider-eval-spinner-type 'half-circle)

;; Use evil-smartparens to make evil play nicer with lispy syntax.
(use-package evil-smartparens
  :hook ((clojure-mode clojure-ts-mode emacs-lisp-mode lisp-mode lisp-interaction-mode lisp-data-mode common-lisp-mode) . evil-smartparens-mode))
;; Recognise lines as text-objects. Enables `vil', `val' etc.
(use-package evil-textobj-line :after evil)

;; PHP + LSP + tree-sitter activation.
(after! eglot
  (add-to-list 'eglot-server-programs
               ;; NOTE License key must be defined before this hook is loaded!
               `(php-mode . ("intelephense" "--stdio"
                             :initializationOptions (:licenceKey ,(bound-and-true-p lsp-intelephense-licence-key))))))

;; Markdown
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

;; Nest Denote notes within the main org directory and include them in agenda.
;; NOTE Make sure org-directory is initialised beforehand!
(when org-directory
  (setq denote-directory (concat org-directory "/notes")
        org-agenda-files (mapcar (lambda (subdir) (concat org-directory subdir))
                                 '("" "notes"))))


;;; File-mode associations

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.bb\\'" . clojure-mode))))


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
(after! evil-escape (setq evil-escape-key-sequence "jk"))
(map! :nvi "M-<left>" #'backward-same-syntax)
(map! :nvi "M-<right>" #'forward-same-syntax)
(map! "M-<backspace>" #'evil-delete-backward-word)
(map! "M-W" (lambda () "Expands selected region once." (interactive) (er--expand-region-1)))
(map! :leader "w O" #'window-next-enlargen)
(map! :leader :desc "Toggle frame decoration" "t d" #'toggle-frame-decoration)
(map! :nvi "C-<tab>" #'other-window)

;; Structural editing
(map! "S-<down>" #'sp-up-sexp
      "S-<up>" #'sp-backward-down-sexp
      "C-M-," #'sp-forward-barf-sexp
      "C-M-." #'sp-forward-slurp-sexp
      "S-M-<up>" #'sp-raise-sexp
      "S-M-<down>" #'sp-splice-sexp
      ;; These *will* mess up terminal input.
      ;; Only activate them in GUI mode.
      (:when (display-graphic-p)
        "M-9" #'sp-wrap-round
        "M-[" #'sp-wrap-square
        "M-{" #'sp-wrap-curly))

;; CIDER
(map! :after cider
      :map clojure-mode-map
      "C-M-<return>" #'cider-eval-sexp-at-point)
(map! :after cider
      :map clojure-mode-map
      :localleader
      "e s" #'cider-eval-sexp-at-point)

;; Denote
(map! :when (require 'denote nil t)
      :leader "n D" #'denote-open-or-create
      :leader (:prefix ("n d" . "Denote")
               :desc "New file" "n" #'denote
               :desc "Browse" "o" (cmd! (dired denote-directory))))

;; Writeroom mode
(map! :after writeroom-mode
      :map writeroom-mode-map
      "C->" #'writeroom-increase-width
      "C-<" #'writeroom-decrease-width)
