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

;;; init.el ends here
