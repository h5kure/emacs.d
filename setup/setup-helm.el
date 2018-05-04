(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
	(eval-when-compile (require 'helm-lib)))

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
	(let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	  (overlay-put ov 'window (selected-window))
	  (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
				  `(:background ,bg-color :foreground ,bg-color)))
	  (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    (setq
     helm-split-window-in-side-p	t
					; open helm buffer inside current window,
					; not occupy whole other window
     helm-move-to-line-cycle-in-source	t
					; move to end or beginning of source when
					; reaching to or bottom of source
     helm-ff-search-library-in-sexp	t
					; search for library in `require' and `declare-function' sexp.
     helm-scroll-amount			8
					; scroll 8 lines other window using M-<next>/M-<prior>
     helm-ff-file-name-history-use-recentf	t
     ;; Allow fuzzy matches in helm semantic
     helm-semantic-fuzzy-match		t
     helm-imenu-fuzzy-match		t
     helm-mode-fuzzy-match		t
     helm-buffers-fuzzy-matching	t
					; fuzzy matching buffer names when non-nil
					; useful in helm-mini that lists buffers
     helm-M-x-fuzzy-match 		t
     helm-lisp-fuzzy-completion		t
     ;; helm-apropos-fuzzy-match 		t
     helm-locate-fuzzy-match		t
     helm-echo-input-in-header-line 	nil

     ;; helm-candidate-number-limit 500
					; limit the number of displayed cadidates
     helm-buffer-skip-remote-checking	t
     helm-org-headings-fontify		t

     helm-display-header-line 		t)

    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    ;;; Have helm automatically resize the window
    (helm-autoresize-mode 1)

    ;;;;;;;;;;;;;;;;;;;
    ;;; KEY BINDING ;;;
    ;;;;;;;;;;;;;;;;;;;

    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h".
    ;; Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c t") 'helm-ctest)
    (global-set-key (kbd "C-c r") 'helm-recentf)
    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
    (global-set-key (kbd "C-c h g") 'helm-google-suggest)
    (global-set-key (kbd "C-c h x") 'helm-register)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return") 'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n") 'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p") 'helm-grep-mode-jump-other-window-backward)

    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
	      #'(lambda ()
		  (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)))

    ;;; save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;; show minibuffer history with Helm
    (define-key global-map [remap find-tab] 'helm-etags-select)

    (define-key global-map [remap list-buffers] 'helm-buffers-list)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; PACKAGE: helm-swoop ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Locate the helm-swoop folder to your path
    (use-package helm-swoop
      :bind (("C-c h o" . helm-swoop)
	     ("C-c s" . helm-multi-swoop-all))
      :config
      ;; when doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows t)

      ;; Split direction. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t))

    (helm-mode 1)

    (use-package helm-projectile
      :init
      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien))))
(provide 'setup-helm)
