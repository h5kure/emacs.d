;;; package setting
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'pakcage-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/setup")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-general-devel)
(require 'setup-c)
(require 'setup-cedet)
(require 'setup-editing)
;; for java
(require 'setup-jdee)
;; for python
(require 'setup-python)

(require 'setup-markdown)

(require 'setup-ecb)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.50")
 '(jdee-server-dir "~/.emacs.d/jdee-server")
 '(package-selected-packages
   (quote
    (elpy zygospore yasnippet ws-butler volatile-highlights use-package undo-tree powerline monokai-theme markdown-mode magit jdee iedit hl-todo helm-swoop helm-projectile helm-gtags helm-company google-c-style dtrt-indent company-c-headers comment-dwim-2 cmake-mode clean-aindent-mode clang-format autopair anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
