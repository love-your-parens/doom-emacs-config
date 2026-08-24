;;; config.linux.el -*- lexical-binding: t; -*-

(map! :when (executable-find "xdg-open")
      :leader

      :desc "Open directory" "o x"
      (lambda ()
        (interactive)
        (shell-command
         (concat "xdg-open "
                 (shell-quote-argument (file-name-directory buffer-file-name)))))

      :desc "Open file" "o X"
      (lambda ()
        (interactive)
        (shell-command
         (concat "xdg-open "
                 (shell-quote-argument buffer-file-name)))))
