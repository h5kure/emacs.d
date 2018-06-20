(use-package ecb
  :init
  (progn
    (require 'ecb)
    (setq ecb-tip-of-the-day nil)
    (setq ecb-windows-width 40)
    (setq ecb-compile-window-height 12)
    (setq ecb-compile-window-temporally-enlarge 'after-selection) ; after-display or after-selection or both or nil
    (setq ecb-compile-window-width 'edit-window) ; frame or edit-window
    (setq ecb-enlarged-compilation-window-max-height 'half) ; best or half or number
    (defun quick-launch-development-mode()
      (interactive)
      (ignore-errors(ecb-activate))
      (elscreen-start)
      )
    (global-set-key (kbd "<f7>") 'quick-launch-development-mode)
    )
  )

(provide 'setup-ecb)
