; connect to a stylish server

(require 'json)
(require 'cl)

(defvar stylish-host "localhost"
  "The host that the Stylish server is running on.")

(defvar stylish-port 36228
  "The port that the Stylish server is running on.")

(defvar stylish-process "stylish"
  "The name of the Stylish process")

(defvar stylish-server-info-alist nil
  "Information about the Stylish server")

(defvar stylish-post-connect-hook nil
  "Hook that is run after connecting (or reconnecting) to the server.")

(defvar stylish-pre-send-command-hook nil
  "Hook that is run before sending a command to the server (but
after the connection is setup, if necessary).")

(defvar stylish-current-token 0
  "The current message token.")

(defvar stylish-outstanding-requests nil
  "A map from token to (callback . state) for currently outstanding requets.")

(defgroup stylish nil
  "Stylish"
  :prefix "stylish-")

(defun stylish-server-version nil (caddr (assoc :version stylish-server-info-alist)))
(defun stylish-session-id nil (cadr (assoc :session-id stylish-server-info-alist)))

(defvar stylish-dispatch-alist nil
  "Map from message type to handler function")
(setq stylish-dispatch-alist
      '((welcome . stylish-handler-welcome)
        (error . stylish-handler-error)))

(defun stylish-connected-p nil
  "Returns T if we are connected to a Stylish process"
  (if (not (ignore-errors (get-process stylish-process))) nil t))

(defun stylish-connect ()
  "Connect to a stylish Stylish server."
  (interactive)
  (get-buffer-create "*inferior stylish json*")
  (let ((p (open-network-stream
            stylish-process "*inferior stylish*"
            stylish-host stylish-port)))
    (set-process-coding-system p 'utf-8 'utf-8)
    (set-process-filter p 'stylish-filter)
    p))

(defun stylish nil
  "Connect to the Stylish server, unless already connected.  You
should run this before sending data to the Stylish server."
  (when (not (stylish-connected-p)) (stylish-connect))
  t)

(defun stylish-json-decode ()
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (with-current-buffer (get-buffer-create "*inferior stylish json*")
      (goto-char (point-min))
      (let ((result (ignore-errors (json-read))))
        (when result
          (delete-region (point) (point-min)))
        result))))

(defun stylish-json-encode (data)
  (json-encode data))

(defun stylish-filter (proc string)
  "Handle responses from the Stylish server.

PROC is the Stylish process, STRING is the message we are trying to parse."
  (with-current-buffer (get-buffer-create "*inferior stylish json*")
    (goto-char (point-max))
    (insert string))
  (while (stylish-handle-one-message)))

(defun stylish-handle-one-message ()
  "Use the JSON buffer to attempt to parse one message."
  (let ((message (stylish-json-decode)))
    (when message
      (let* ((type (intern (getf message :producer)))
             (args (getf message :results))
             (token (string-to-int (getf args :token)))
             (handler (assoc token stylish-outstanding-requests))
             (general-handler (assoc type stylish-dispatch-alist)))
        (unwind-protect
            (condition-case error
                (cond (handler
                       (destructuring-bind (token function state) handler
                         (funcall function state args)))
                      (general-handler (apply (cdr general-handler) args))
                      (t (error "No handler for message type %s" type)))
              (error
               (message "Error in stylish filter: %s %s [token %s]" (car error) (cdr error) token)))
          (setf stylish-outstanding-requests
                (assq-delete-all token stylish-outstanding-requests)))))
    message))

(defun stylish-register-handler (action handler)
  "Register a HANDLER for ACTION."
  (add-to-list 'stylish-dispatch-alist (cons action handler)))

(defun* stylish-handler-welcome (&key version sessionid)
  "Show welcome message after connecting to Stylish server"
  (setq stylish-server-info-alist (list :version version :session-id sessionid))
  (run-hooks 'stylish-post-connect-hook)
  (message "Connected to %s (sessionid %s)" version sessionid))

(defun* stylish-handler-error (&key message)
  "Handle an error returned by the Stylish server."
  (message "Error from Stylish: %s" message))

(defun stylish-send-command (command callback state &rest args)
  "Send a COMMAND with ARGS to the Stylish server.
Argument CALLBACK is an optional callback that will be called with STATE and the result of the function."
  (stylish)
  (run-hooks 'stylish-pre-send-command-hook)
  (setq stylish-partial-message nil) ; cancel partial message
  (incf stylish-current-token)
  (setf args (nconc args (list :token stylish-current-token)))

  (when callback
    (setf stylish-outstanding-requests
          (nconc stylish-outstanding-requests
                 (list (list stylish-current-token callback state)))))

  (let ((json (stylish-json-encode (list :type command :payload args))))
    (process-send-string "stylish" (concat json "\n")))
  stylish-current-token)

(defun* stylish-handler-load-plugin (plugin (&key id token))
  (message "Loaded plugin %s ok" plugin))

(defun stylish-load-plugin (plugin)
  "Load a server-side Stylish plugin PLUGIN."
  (stylish-send-command "load_plugin"
                        #'stylish-handler-load-plugin plugin :plugin plugin))

(provide 'stylish)

;(stylish)
;(stylish-send-command 'foo 1 2 3)
;(setq debug-on-error nil)