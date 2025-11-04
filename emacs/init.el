;; -*- lexical-binding: t; -*-
(defvar temporary-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      gcmh-high-cons-threshold (* 1024 1024 1024)
      gcmh-idle-delay-factor 20
      jit-lock-defer-time 0.05
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024)
                           gc-cons-percentage 0.1
                           file-name-handler-alist temporary-file-name-handler-alist)))

                                        ; load straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; We need to fetch the use-package version of org before evaluating the literate files.
(straight-use-package 'org)

;; load literate configuration files
(defun aleks/load-org-file (name)
  "Load an org file with NAME from the org directory."
  (let* ((config-emacs-directory "~/config/emacs")
         (org-file (expand-file-name name config-emacs-directory)))
    (if (file-readable-p org-file)
        (org-babel-load-file org-file))))

(aleks/load-org-file "private.org")
(aleks/load-org-file "main.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
