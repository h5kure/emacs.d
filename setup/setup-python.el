;;; Use Elpy for python development environment
;;; Required rope, jedi, flake8, autopep8, and yapf
;;; using pip

(use-package elpy
  :init
  (setq elpy-rpc-python-command "/usr/local/bin/python3")
  ;; (setq elpy-rpc-pythonpath "/usr/local/bin/python3")
  (elpy-enable)
  )

(setq python-shell-interpreter "python3")
(setq python-indent-offset 4)
(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand))
  )
(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

(provide 'setup-python)
