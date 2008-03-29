(require 'stylish)

(stylish-register-handler 'highlight 'stylish-handler-highlight)

(defvar stylish-pending-highlight-requests nil
  "An alist of (tag . buffer) pairs waiting for syntaxification
results to come back.")

(defvar stylish-faces nil
  "An alist of (type . face) pairs specifying fontification rules")
(setq stylish-faces
      (list
       (cons 'module-name font-lock-function-name-face)
       (cons 'keyword font-lock-keyword-face)))

(defun stylish-syntaxify (start end &optional buffer)
  "Send BUFFER's text between START and END to Stylish for fontification."
  (when (not buffer) (setq buffer (current-buffer)))
  (remove-text-properties start end '(face nil) buffer)
  (let ((text (buffer-substring-no-properties start end))
        (tag (random)))
    (while (string-match "\n" text)
      (setq text (replace-match " " nil nil text)))
    (add-to-list 'stylish-pending-highlight-requests (cons tag buffer))
    (stylish-send-command 'highlight tag start text)))

(defun stylish-handler-highlight (result)
  "Handle the syntaxification result."
  (let* ((tag    (cadr (assoc 'tag result)))
         (data   (cadr (assoc 'result result)))
         (buffer (cdr (assoc tag stylish-pending-highlight-requests))))
    ;(delq (cons tag buffer) stylish-pending-highlight-requests)
    (setq stylish-pending-highlight-requests nil)
    (with-current-buffer buffer
      (loop for rule in data
            do (let* ((start  (caar rule))
                      (end    (cadar rule))
                      (syntax (cadr rule))
                      (face   (or (cdr (assoc syntax stylish-faces))
                                  font-lock-warning-face)))
                 (add-text-properties start end (list 'face face) buffer))))))

(defvar last-syntax 0)
(defun stylish-do-current-line nil
  (interactive)
  (make-local-variable 'last-syntax)
  (when (> (- (cadr (current-time)) last-syntax) 1)
    (setq last-syntax (cadr (current-time)))
    (stylish-syntaxify (save-excursion (beginning-of-line) (point))
                       (save-excursion (end-of-line) (point))
                       (current-buffer))))

(defun turn-on-stylish-syntax nil
  "Turn on stylish syntaxifier"
  (interactive)
  (add-hook 'post-command-hook 'stylish-do-current-line nil t))
