(require 'stylish)

(stylish-register-handler 'repl 'stylish-handler-repl)

(defun stylish-handler-repl (status result)
  "Handle a return from the REPL"
  (insert result)
  (insert "\n")
  (insert-stylish-repl-prompt))

(defun stylish-repl-send nil
  "Send a command to the REPL"
  (interactive)
  (save-excursion
    (re-search-backward "PERL> ")
    (let ((command (buffer-substring-no-properties 
                    (match-end 0)
                    (save-excursion (end-of-line) (point)))))
      (stylish-send-command 'repl command))))

(defun stylish-repl nil
  (interactive)
  (switch-to-buffer "*Stylish REPL*")
  (stylish-repl-mode))

(define-derived-mode stylish-repl-mode fundamental-mode "Stylish[REPL]"
  "The major mode for the Stylish REPL buffer."
  (stylish)
  (insert "Welcome to the Stylish REPL!\n")
  (insert-stylish-repl-prompt)
  (local-set-key (kbd "<enter>") 'stylish-repl-send))

(defun insert-stylish-repl-prompt nil
  (insert (propertize "PERL>" 'face font-lock-keyword-face))
  (insert " "))
