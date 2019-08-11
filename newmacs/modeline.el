;;; modeline --- Themeing configuration

;;; Commentary:

;;; Code:

(use-package all-the-icons)

(setq mode-line-format
      '("%e"
	mode-line-front-space
	mode-line-client
	mode-line-modified
	mode-line-remote
	mode-line-frame-identification
	mode-line-buffer-identification
	(propertize (all-the-icons-octicon "package")
		    'face `(:family  ,(all-the-icons-octicon-family) :height 1.2)
		    'display '(raise 0.1))
	"   "
	(vc-mode vc-mode)
	"  "
	mode-line-misc-info
	evil-mode-line-tag
	mode-line-position
	mode-line-end-spaces))

; (all-the-icons-insert-icons-for 'octicon 10)


(insert (propertize (all-the-icons-octicon "package")
		    'face `(:family  ,(all-the-icons-octicon-family) :height 1.2)
		    'display '(raise 0.1)))
;;; modeline.el ends here
