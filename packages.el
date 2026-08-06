;; -*- lexical-binding: t; -*-

;; https://github.com/rejeep/drag-stuff.el
(package! drag-stuff)

;; https://github.com/protesilaos/denote
(package! denote)

;; https://github.com/expez/evil-smartparens
(package! evil-smartparens)

;; https://github.com/emacsorphanage/evil-textobj-line?tab=readme-ov-file
(package! evil-textobj-line)

(package! rainbow-mode)

;; https://github.com/tarsius/paren-face
(package! paren-face)

;; https://github.com/alphapapa/topsy.el
(package! topsy)

;; Use Delta if available (cleaner Git diffs).
;; https://dandavison.github.io/delta/introduction.html
;; https://github.com/dandavison/magit-delta
(when (executable-find "delta") (package! magit-delta))

;; MacOS-specific packages.
(when (featurep :system 'macos) (load! "packages.macos.el"))
