; connect to a stylish server

(defvar stylish-host "localhost"
  "The host that the Stylish server is running on")

(defvar stylish-port 36227
  "The port that the Stylish server is running on")

(defvar stylish-process "stylish"
  "The name of the Stylish process")

(defvar stylish-server-info-alist nil
  "Information about the Stylish server")

(defvar stylish-partial-message nil
  "Long messages get split; this variable accumulates partial
messages until we get a full one.")

(defvar stylish-post-connect-hook nil
  "Hook that is run after connecting (or reconnecting) to the server.")

(defvar stylish-pre-send-command-hook nil
  "Hook that is run before sending a command to the server (but
after the connection is setup, if necessary).")

(defgroup stylish nil
  "Stylish"
  :prefix "stylish-")

(defun stylish-server-version nil (caddr (assoc :version stylish-server-info-alist)))
(defun stylish-session-id nil (cadr (assoc :session-id stylish-server-info-alist)))

(defvar stylish-dispatch-alist nil
  "Map frommessage type to handler function")
(setq stylish-dispatch-alist 
      '((welcome . stylish-handler-welcome)
        (error . stylish-handler-error)))

(defun stylish-connected-p nil
  "Returns true if we are connected to a Stylish process"
  (if (not (ignore-errors (get-process stylish-process))) nil t))

(defun stylish-connect ()
  "Connect to a stylish Stylish server"
  (interactive)
  (setq stylish-partial-message nil)
  (condition-case nil
      (let ((p (open-network-stream 
                stylish-process "*inferior stylish*" 
                stylish-host stylish-port)))
        (set-process-coding-system p 'utf-8 'utf-8)
        (set-process-filter p 'stylish-filter)
        p)
    (error (setq stylish-server-info-alist nil)
           (error "Failed to connect to the Stylish server!"))))

(defun stylish nil
  "Connect to the Stylish server, unless already connected.  You
should run this before sending data to the Stylish server."
  (when (not (stylish-connected-p)) (stylish-connect))
  t)

(defun stylish-filter (proc string)
  (let* ((attempt (concat stylish-partial-message string)) 
         (message (ignore-errors (read attempt))))
    (if (not message)

        ;; didn't get the whole message yet
        (setq stylish-partial-message attempt)

      ;; read was OK
      (setq stylish-partial-message nil)
      (let* ((type (car message))
             (args (cdr message))
             (handler (assoc type stylish-dispatch-alist)))
        ;;(message "Handling %s via %s" message (prin1-to-string (cdr handler)))
        (condition-case error
            (if handler 
                (apply (cdr handler) args)
              (error "No handler for message type %s" type))
          (error 
           (message "Error in stylish filter %s: %s" (car error) (cdr error))))))))

(defun stylish-register-handler (action handler)
  "Register an (action . handler)."
  (add-to-list 'stylish-dispatch-alist (cons action handler)))

(defun stylish-handler-welcome (information)
  "Show welcome message after connecting to Stylish server"
  (let ((session-id (cadr (assoc :session-id information)))
        (version    (cdr  (assoc :version information))))
    (run-hooks 'stylish-post-connect-hook)
    (setq stylish-server-info-alist information)
    (message "Connected to %s %s (sessionid %s)" 
             (car version) (cadr version) session-id)))

(defun stylish-handler-error (type message)
  "Handle an error returned by the Stylish server"
  ;(message "Error from Stylish (%s): %s" type message))
  )

(defun stylish-send-command (command &rest args)
  "Send a COMMAND with ARGS to the Stylish server.  The result is returned
asynchronously.  See [stylish-filter] and [stylish-dispatch-alist]."
  (stylish)
  (run-hooks 'stylish-pre-send-command-hook)
  (setq stylish-partial-message nil) ; cancel partial message
  (let ((message (prin1-to-string (cons command args))))
    (process-send-string "stylish" (format "%s\n" message))))

(provide 'stylish)
               
;(stylish)
;(stylish-send-command 'foo 1 2 3)
;(setq debug-on-error nil)