(let* ((config-emacs-directory "~/config/newmacs")
       (main-org-file (expand-file-name "main.org" config-emacs-directory))
       (private-org-file (expand-file-name "private.org" config-emacs-directory)))
  (if (file-readable-p main-org-file)
      (org-babel-load-file main-org-file))
  (if (file-readable-p private-org-file)
      (org-babel-load-file private-org-file)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-mode tide flycheck company which-key counsel-projectile projectile hydra counsel ivy evil-magit magit smartparens use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
