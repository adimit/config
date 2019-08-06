(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(package-initialize)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :weight 'normal
                    :width 'normal
                    :height 110)

(load-theme 'wombat)

(blink-cursor-mode 0)

(defun my-keybindings ()
  (defhydra hydra-comma ()
    "main menu"
    ("b" counsel-switch-buffer "switch buffer")
    ("p" projectile-command-map "project…")
    ("f" counsel-find-file "open file"))
  (define-key evil-normal-state-map "," 'hydra-comma/body))

(use-package evil
:config
  (evil-mode t))

(use-package ivy)
(use-package counsel)
(use-package hydra
  :config (my-keybindings))

(use-package projectile
  :config (projectile-mode +1))
(use-package counsel-projectile)

(use-package magit)
(use-package evil-magit)

(use-package smartparens
  :config
  (smartparens-global-mode t))

(add-hook 'org-mode-hook 'org-indent-mode)
