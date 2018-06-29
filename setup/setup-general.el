(menu-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default window size when start-up ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maximize
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup Emacs default settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t) ; Don't want any startup message
(setq make-backup-files nil) ; Don't want any backup files
(setq auto-save-list-file-name nil) ; Don't want any .saves files
(setq auto-save-default nil) ; Don't want any auto saving
(setq search-highlight t) ; Highlight search object
(setq query-replace-highlight t) ; highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening
(setq transient-mark-mode t) ; marking a region will be highlight
(global-hl-line-mode 1) ; highlight line the cursor is on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add these to the PATH so that proper executables are found on OS X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq exec-path (append exec-path '("/usr/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;;;;;;;;;;;;;;;;;;;
;;; spaces setting ;;;
;;;;;;;;;;;;;;;;;;;;;;
;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; show unnecessary whitesapce that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;;;;;;;;;;;;;;;;;;;;
;;; Set up Theme ;;;
;;;;;;;;;;;;;;;;;;;;
;;; set default theme
;; (use-package monokai-theme
;;   :init
;;   (load-theme 'monokai t)
;;   )
;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t)
;;   )
(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox t)
  )
;;; use powerline
(use-package powerline
  :init
  (require 'powerline)
  (powerline-default-theme))

;;;;;;;;;;;;;;;;
;;; autopair ;;;
;;;;;;;;;;;;;;;;
(use-package autopair
  :init
  (autopair-global-mode)
  (show-paren-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line number setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode t) ; line number
(unless window-system
  (add-hook 'linum-before-numbering-hook
                (lambda ()
                        (setq-local linum-format-fmt
                                      (let ((w (length (number-to-string
                                                            (count-lines (point-min) (point-max))))))
                                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))
;; line num off
(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode twittering-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (and linum-disable-starred-buffers (string-match "*" (buffer-name)))
              )
    (linum-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move between window easily
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connect remote server easily.
;; don't ask password again when open remote.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq password-cache-expiry nil)
(defun open-remote (dest)
  "Open remote host home by find-file."
  (interactive "sDest: ")
  (let* ((switches (format "/ssh:%s:~/" dest)))
    (find-file switches)
    )
  )
(global-set-key (kbd "<f2>") 'open-remote)
(defun my-ssh (user host port)
  "Connect to a remote host by SSH."
  (interactive "sUser: \nsHost: \nsPort (default 22): ")
  (let* ((port (if (equal port "") "22" port))
         (switches (list host "-l" user "-p" port)))
    (set-buffer (apply 'make-term "ssh" "ssh" nil switches))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer "*ssh*")))
(global-set-key (kbd "<f3>") 'my-ssh)


;;;;;;;;;;;;;;;;
;; setup font
;;;;;;;;;;;;;;;;
;; to install Source Code Pro fonts in OS X
;; brew tap caskroom/fonts && brew cask install font-source-code-pro
(defun font-exists-p (font) "check if font exists" (if (null (x-list-fonts font)) nil t))
(if (font-exists-p "Source Code Pro") (set-face-attribute 'default nil :font "Source Code Pro"))
;; (add-to-list 'default-frame-alist
;;              '(font . "Source Code Pro for Powerline"))

;;;;;;;;;;;;;;;;;;;;;;;
;; setup transparency
;;;;;;;;;;;;;;;;;;;;;;;
;; (set-frame-parameter (selected-frame) 'alpha '(active . inactive))
;; (add-to-list 'default-frame-alist '(alpha . <both>))
(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 85)))

(provide 'setup-general)
