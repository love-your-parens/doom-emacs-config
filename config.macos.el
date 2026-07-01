;;; config.macos.el -*- lexical-binding: t; -*-

;;; MacOS platform-specific settings.

(setq consult-locate-args "mdfind" ; supplants /bin/locate - preferred in OSX
      lsp-file-watch-threshold 10000 ; guards against exhausting the open-file limit in OSX
      )

;; Make sure PATH is correctly loaded in in GUI mode.
;; NOTE unnecessary if emacs-plus was built with inject_path
(when (and (memq window-system '(ns mac))
           (featurep 'exec-path-from-shell))
  (exec-path-from-shell-initialize))

;; Handle non-POSIX shells cleanly:
(setq shell-file-name (executable-find "zsh"))
(setq-default vterm-shell "/opt/homebrew/bin/fish")
(setq-default explicit-shell-file-name "/opt/homebrew/bin/fish")

;; NOTE Assume that if Lua LSP is in use, it will have been installed using Homebrew.
;; For that to work with Eglot, default paths need to be changed. In particular:
;; `+lua-lsp-dir' needs to be pointing to the same directory that `main.lua' is in.
;; Since this path depends on versioning, we must locate it by pattern.
;; NOTE Observed to add ~10ms to the startup.
(when (and (modulep! :lang lua +lsp)
           (modulep! :tools lsp +eglot))
  (let ((lua-server-path
         (let ((filename (with-temp-buffer
                           (call-process-shell-command
                            "find /opt/homebrew/Cellar/lua-language-server/ -path '*/libexec/main.lua' 2>/dev/null | head -n1"
                            nil t)
                           (buffer-string))))
           (when (not (string-empty-p filename))
             (file-name-parent-directory filename)))))
    (setq +lua-lsp-dir lua-server-path)))
