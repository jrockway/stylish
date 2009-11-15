;;; stylish-inpackage.el --- show current package in the minibuffer

(define-minor-mode cperl-inpackage "" nil "" nil
  ; (stylish-load-plugin "InPackage")
  (stylish-inpackage-show-package)
  (add-hook 'post-command-hook #'stylish-inpackage-show-package nil t))

(defun stylish-inpackage-show-package ()
  (stylish-send-command "in_package" #'stylish-inpackage-update-modeline nil :file (buffer-file-name) :line (line-number-at-pos)))

(defun* stylish-inpackage-update-modeline (state (&key token package))
  (setf mode-line-process (concat " " package)))



(provide 'stylish-inpackage)
;;; stylish-inpackage.el ends here
