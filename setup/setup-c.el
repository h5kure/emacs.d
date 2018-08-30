(use-package rtags
  :ensure t
  :init
  :config
  (progn
    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
    (rtags-enable-standard-keybindings)
    (setq rtags-use-helm t)
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
    )
  )

(use-package cmake-ide
  :ensure t
  :config
  :after (rtags)
  (add-hook 'c-mode-hook 'cmake-ide-setup)
  (add-hook 'c++-mode-hook 'cmake-ide-setup)
  :bind (("C-c C-a" . cmake-ide-compile))
  )

(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
    (setq company-idel-delay 0))
  )

(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode))
  )

(use-package irony
  :ensure t
  :config
  (progn
    ;; if irony server was never installed, install it.
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)

    ;; Use compilation database first, clang_complete as fallback.
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))

(use-package company-irony
  :ensure t
  :after (company irony)
  :config
  (progn
    (add-to-list 'company-backends 'company-keywords)
    (add-to-list 'company-backends 'company-irony)
    (add-to-list 'company-backends 'company-irony-c-headers)
    ))

(use-package flycheck-irony
  :ensure t
  :after (flycheck irony)
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  )

(use-package irony-eldoc
  :ensure t
  :after (eldoc irony)
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc))
  )

(use-package helm-rtags
  :after (helm rtags)
  :config
  (progn
    (setq rtags-display-result-backend 'helm)))

(use-package company-rtags
  :after (company rtags)
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)))

(use-package flycheck-rtags
  :after (flycheck rtags)
  :config
  (progn
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil)
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)
      )
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
    ))

(use-package projectile
  :config
  (progn
    (projectile-global-mode)))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

;; company-c-headers
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux") ;; set style to "linux"
(use-package google-c-style
  :init
  (add-hook 'c-mode-hook 'google-set-c-style)
  (add-hook 'c-mode-hook 'google-make-newline-indent)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-make-newline-indent)
  )

;; clang-format can be triggered using C-M-tab
(use-package clang-format
  :init
  (require 'clang-format)
  (global-set-key [C-M-tab] 'clang-format-region)
  ;; Create clang-format file using google style
  ;; clang-format -style=google -dump-config > .clang-format
  (setq clang-format-style-option "google")
  )


(use-package cc-mode
  :init
  (require 'cc-mode)
  (define-key c-mode-map  [(control tab)] 'helm-company)
  (define-key c++-mode-map  [(control tab)] 'helm-company)
  )

(provide 'setup-c)
