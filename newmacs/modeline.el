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
	(propertize (format "%s" (all-the-icons-icon-for-file "foo.js"))
		    'face `(:height 1)
		    'display '(raise 0))
	mode-line-buffer-identification
	"   "
	(vc-mode vc-mode)
	"  "
	mode-line-misc-info
	evil-mode-line-tag
	mode-line-position
	mode-line-end-spaces))

;;; modeline.el ends here
