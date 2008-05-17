;;; interactive code creation with stylish-repl

(require 'stylish)
(require 'stylish-repl)

(defvar stylish-repl-ie--editing nil
  "The list of currently-being-edited lines")

(defvar stylish-repl-ie--update-edit-next nil
  "Rewrite the history the next time the send hook is run?")

(defun stylish-repl-ie-commit-line () 
  "Commit the line last entered to the editing block."
  (let ((last-line (stylish-repl-history-get 1)))
    (unless last-line (error "No line to add!"))
    (setq stylish-repl-ie--editing
          (append stylish-repl-ie--editing (list last-line)))
    (stylish-repl-ie-list))
  t) ; print the prompt

(stylish-repl-register-command "ci" 'stylish-repl-ie-commit-line)

(defun stylish-repl-ie-list nil
  "Show the current list of lines"
  (when stylish-repl-ie--editing
    (with-temp-buffer
      (ignore-errors (let (cperl-mode-hook) (cperl-mode)))
      (loop for line in stylish-repl-ie--editing
            do (goto-char (point-max)) (insert line) (insert "\n"))
      (font-lock-fontify-buffer)
      (let (msg (count 0))
        (goto-char (point-min))
        (while (re-search-forward "^\\(.+\\)$" nil t)
          (setq msg (format "%s%d: %s\n" (or msg "") (incf count) (match-string 1))))
        (with-current-buffer "*Stylish REPL*"
          (stylish-repl-insert msg)))))
  t)

(stylish-repl-register-command "list" 'stylish-repl-ie-list)

(defun stylish-repl-ie-replace-last-line nil
  "Yank the last edit line to the prompt, allow editing, and
replace that line with the edited version when the line is sent to
the REPL."
  (stylish-repl-insert-prompt)
  (insert (car (reverse stylish-repl-ie--editing)))
  (setq stylish-repl-ie--update-edit-next t)
  nil)

(stylish-repl-register-command "edit" 'stylish-repl-ie-replace-last-line)
(add-hook 'stylish-repl-send-hook
          (lambda nil
            (when stylish-repl-ie--update-edit-next
              (setq stylish-repl-ie--update-edit-next nil)
              (setq stylish-repl-ie--editing 
                    (append (reverse (cdr (reverse stylish-repl-ie--editing)))
                            (list text))))))

(defun stylish-repl-ie-delete-last-line nil
  "Delete the most recently added line."
  (setq stylish-repl-ie--editing (reverse (cdr (reverse stylish-repl-ie--editing))))
  (stylish-repl-ie-list)
  t)

(stylish-repl-register-command "delete" 'stylish-repl-ie-delete-last-line)

(defun stylish-repl-ie-forget nil
  "Clear all lines."
  (setq stylish-repl-ie--editing nil) t)

(stylish-repl-register-command "forget" 'stylish-repl-ie-forget)

(defun stylish-repl-ie-to-block (&optional join-char)
  "Convert the entire list of lines to a code block."
  (let (msg)
    (loop for line in stylish-repl-ie--editing
          do (setq msg (concat msg line (or join-char " "))))
    msg))

(defun stylish-repl-ie-run nil
  "Send the entire list of lines to perl to evaluate."
  (let ((text (format "sub { %s }->();" (stylish-repl-ie-to-block ";"))))
    (stylish-send-command 'repl text))
  nil)

(stylish-repl-register-command "run" 'stylish-repl-ie-run)


(defun stylish-repl-yank-handler (string)
  "Called when the edited lines are yanked into a buffer."
  (save-excursion
    (let ((start (point)) end)
      (insert string)
      (setq end (point))
      (indent-region start end)
      (goto-char end))
    (stylish-repl-ie-clear)))

(defun stylish-repl-ie-copy-to-kill nil
  "Copies the current editing text to the kill ring.  When you
yank this into a buffer, the editing history will be erased."
  (kill-new (stylish-repl-ie-to-block "\n") nil 
            '(stylish-repl-yank-handler))
  t)

(stylish-repl-register-command "kill" 'stylish-repl-ie-copy-to-kill)

(provide 'stylish-repl-iedit)
