;;; The JDEE is an add-on software package that turns Emacs into a comprehensive system for creating, editing, debugging, and documenting Java applications.

;;; required jdee-server.
;;; https://github.com/jdee-emacs/jdee-server.git

(use-package jdee
  :init
  (custom-set-variables
   '(jdee-server-dir "~/.emacs.d/jdee-server"))
  (add-hook 'java-mode-hook 'google-set-c-style)
  (add-hook 'java-mode-hook 'google-make-newline-indent)
  )


(provide 'setup-jdee)
