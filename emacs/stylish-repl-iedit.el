;;; interactive code creation with stylish-repl

(require 'stylish)
(require 'stylish-repl)

(defvar stylish-repl-ie--current-edit nil
  "The list of currently-being-edited lines")

(defun stylish-repl-ie-commit-line () 
  "Commit the line last entered to the editing block."
  (let ((last-line (stylish-repl-history-get 1)))
    (unless last-line (error "No line to add!"))
    (setq stylish-repl-ie--current-edit
          (append stylish-repl-ie--current-edit (list last-line)))
    (let (msg (count 0))
      (loop for line in stylish-repl-ie--current-edit
            do (setq msg (format "%s%d: %s\n" (or msg "") (incf count) line)))
      (stylish-repl-message msg)))
  t) ; print the prompt

(stylish-repl-register-command "ci" 'stylish-repl-ie-commit-line)

(provide 'stylish-repl-iedit)