;;; synced/early-init.el -*- lexical-binding: t; -*-

;; Big performance impact.
;; See: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
;; NOTE May require that the LSP module is rebuilt.
(setenv "LSP_USE_PLISTS" "true")
