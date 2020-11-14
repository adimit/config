;;; package -- Init file for emacs
;;; Commentary:
;;; This file is just the barebones initialisation.  It loads org mode
;;; and compiles the required main.org file.  There are some performance
;;; improvements as well.

;;; Code:

; Work around the GC on startup
; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(defvar temporary-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun aleks/load-org-file (name)
  "Load an org file with NAME from the org directory."
  (let* ((config-emacs-directory "~/config/newmacs")
         (org-file (expand-file-name name config-emacs-directory)))
    (if (file-readable-p org-file)
        (org-babel-load-file org-file))))

(aleks/load-org-file "main.org")

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1
                           file-name-handler-alist temporary-file-name-handler-alist)))

;;; init.el ends here
