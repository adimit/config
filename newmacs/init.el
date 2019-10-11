; Work around the GC on startup
; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(defvar temporary-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)


(let* ((config-emacs-directory "~/config/newmacs")
       (main-org-file (expand-file-name "main.org" config-emacs-directory))
       (private-org-file (expand-file-name "private.org" config-emacs-directory)))
  (if (file-readable-p main-org-file)
      (org-babel-load-file main-org-file))
  (if (file-readable-p private-org-file)
      (org-babel-load-file private-org-file)))

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1
                           file-name-handler-alist temporary-file-name-handler-alist)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-backend (quote ivy))
 '(package-selected-packages
   (quote
    (yaml-mode json-mode dockerfile-mode csv-mode scss-mode diff-hl diff-hl-mode all-the-icons company-prescient ivy-prescient prescient general web-mode tide flycheck company which-key counsel-projectile projectile hydra counsel ivy evil-magit magit smartparens use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
