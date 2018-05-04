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
