(stylish-register-handler 'docstring 'stylish-docstring--display-docstring)

(defun stylish-docstring-lookup-attribute (class attribute)
  "Lookup the documentation for ATTRIBUTE, an attribute in CLASS."
  (interactive "sClass: \nsAttribute: ")
  (stylish-send-command 'docstring class "attribute" attribute))

(defun stylish-docstring--display-docstring (docs)
  "Display a docstring."
  (message docs))
