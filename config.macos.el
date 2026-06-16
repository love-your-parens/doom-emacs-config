;;; config.macos.el -*- lexical-binding: t; -*-

;;; MacOS platform-specific settings.

(setq consult-locate-args "mdfind" ; supplants /bin/locate - preferred in OSX
      lsp-file-watch-threshold 10000 ; guards against exhausting the open-file limit in OSX
      )

;; Make sure PATH is correctly loaded in in GUI mode.
(when (memq window-system '(ns mac))
  (exec-path-from-shell-initialize))

;; Handle non-POSIX shells cleanly:
(setq shell-file-name (executable-find "zsh"))
(setq-default vterm-shell "/opt/homebrew/bin/fish")
(setq-default explicit-shell-file-name "/opt/homebrew/bin/fish")
