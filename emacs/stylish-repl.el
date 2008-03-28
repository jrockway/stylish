(require 'stylish)

(stylish-register-handler 'repl 'stylish-handler-repl)

(defun stylish-handler-repl (status result)
  "Handle a return from the REPL"
  (with-current-buffer (get-buffer "*Stylish REPL*")
    (goto-char (point-max))
    (insert result)
    (insert "\n")
    (insert-stylish-repl-prompt)
    (goto-char (point-max))))

(defun stylish-repl-send nil
  "Send a command to the REPL"
  (interactive)
  (save-excursion
    (re-search-backward "PERL> ")
    (let ((command (buffer-substring-no-properties 
                    (match-end 0)
                    (save-excursion (end-of-line) (point)))))
      (stylish-send-command 'repl command)))
  (insert "\n"))

(defun stylish-repl-send-file (&optional buffer)
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (let ((fn (buffer-file-name buffer)))
    (stylish-send-command 'repl-load-file fn)
    (with-current-buffer (get-buffer "*Stylish REPL*")
      (insert (format "\n# Sending %s\n" fn)))))

(defun stylish-repl nil
  (interactive)
  (switch-to-buffer "*Stylish REPL*")
  (stylish-repl-mode))

(define-derived-mode stylish-repl-mode fundamental-mode "Stylish[REPL]"
  "The major mode for the Stylish REPL buffer."
  (stylish)
  (insert "Welcome to the Stylish REPL!\n")
  (message "Let the hacking commence!")
  (insert-stylish-repl-prompt)
  (local-set-key (kbd "C-c C-c") 'stylish-repl-send))

(defun insert-stylish-repl-prompt nil
  (insert (propertize "PERL>" 'face font-lock-keyword-face))
  (insert " "))
