(require 'stylish)

(stylish-register-handler 'repl 'stylish-handler-repl)

(defvar stylish-repl-history nil
  "History of commands you've entered into the REPL.")

(defvar stylish-repl-prompt-map nil
  "The keymap when at the PERL> prompt")

(setq stylish-repl-prompt-map 
      (let ((k (make-sparse-keymap)))
        (define-key k (kbd "<RET>") 'stylish-repl-send)
        (define-key k (kbd "C-a") 'stylish-repl-beginning-of-line)
        k))

; custom

(defgroup stylish-repl nil
  "Stylish REPL"
  :prefix "stylish-repl-"
  :group 'stylish)

(defface stylish-repl-result-face
  '((t (:inherit font-lock-type-face)))
  "The face for the result of a REPL evaluation"
  :group 'stylish-repl)

(defface stylish-repl-error-face
  '((t (:inherit font-lock-warning-face)))
  "The face for a perl error returned as the result of a REPL evaluation"
  :group 'stylish-repl)

(defface stylish-repl-message-face
  '((t (:inherit font-lock-comment-face)))
  "The face for messages generated by the Stylish REPL"
  :group 'stylish-repl)

(defface stylish-repl-sent-face 
  '((t (:underline t)))
  "The face the query is changed to after its sent to the REPL."
  :group 'stylish-repl)

(defun stylish-repl nil
  "Spawn a Stylish REPL buffer"
  (interactive)
  (switch-to-buffer "*Stylish REPL*")
  (stylish-repl-mode))

(define-derived-mode stylish-repl-mode fundamental-mode "Stylish[REPL]"
  "The major mode for the Stylish REPL buffer."
  (stylish) ; XXX: version number isn't picked up if we reconnect here
  (stylish-repl-insert 
   (format "Welcome to the Stylish REPL! (Stylish %s - id %s)\n"
           (stylish-server-version) (stylish-session-id))
   'stylish-repl-message-face)
  (message "Let the hacking commence!")
  (insert-stylish-repl-prompt))

(defun stylish-repl-usual-properties (start end &optional face)
  (let ((inhibit-read-only t)) ; fuck you, read-only.
    (when face
      (put-text-property start end 'face face))
    (put-text-property start end 'read-only t)
    (put-text-property start end 'intangible t)
    (put-text-property start end 'rear-nonsticky '(read-only face intangible))))

(defun stylish-repl-insert (text &optional face)
  "Insert immutable text into the Stylish REPL buffer"
  (let ((inhibit-read-only t) (begin (point)))
    (insert text)
    (stylish-repl-usual-properties begin (point) face)))

(defun stylish-handler-repl (status result)
  "Handle a return from the REPL"
  (with-current-buffer (get-buffer "*Stylish REPL*")
    (save-excursion 
      (goto-char (point-max))
      (let ((face (if (eq status :error) 
                      'stylish-repl-error-face 'stylish-repl-result-face)))
        (stylish-repl-insert (concat result "\n") face))
      (insert-stylish-repl-prompt))
    (goto-char (point-max))))

(defun stylish-repl-input-region nil
  "Determine the Stylish input region"
  ;; XXX: rewrite this to use properties instead of regexen!
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (re-search-backward "PERL> "))
    (let ((start (match-end 0))
          (end   (save-excursion (end-of-line) (point))))
      (cons start end))))

(defun stylish-repl-send nil
  "Send a command to the REPL"
  (interactive)
  (let* ((region (stylish-repl-input-region))
         (start (car region))
         (end (cdr region)))
    (stylish-repl-usual-properties start end 'stylish-repl-sent-face)
    (stylish-send-command 'repl (buffer-substring-no-properties start end)))
  (stylish-repl-insert "\n"))

(defun stylish-repl-send-file (&optional buffer)
  "Send a file to the REPL to load"
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (let ((fn (buffer-file-name buffer)))
    (stylish-send-command 'repl-load-file fn)
    (with-current-buffer (get-buffer "*Stylish REPL*")
      (stylish-repl-insert
       (format "\n# Sending %s\n" fn) 'stylish-repl-message-face))))

(defun insert-stylish-repl-prompt nil
  "Insert the REPL prompt"
  (stylish-repl-insert "PERL>" font-lock-keyword-face)
  (stylish-repl-insert (propertize " " 'local-map stylish-repl-prompt-map)))

(defun stylish-repl-beginning-of-line nil
  (interactive)
  (goto-char (car (stylish-repl-input-region))))
