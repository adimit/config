; package
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; require evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map ";" 'evil-ex)
(add-to-list 'load-path "~/.emacs.d/evil-leader")
(require 'evil-leader)
(evil-leader/set-leader ",")
(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'surround)
(global-surround-mode 1)
(add-to-list 'load-path "~/.emacs.d/evil-numbers")
(require 'evil-numbers)
(evil-leader/set-key "na" 'evil-numbers/inc-at-pt)
(evil-leader/set-key "nx" 'evil-numbers/dec-at-pt)

; ido
(require 'ido)
(ido-mode t)
(evil-leader/set-key "b" 'ido-switch-buffer)
(evil-leader/set-key "e" 'ido-find-file)

; Spell Czech

;; Switch through languages
(let ((langs '("british" "de")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key [f6] 'cycle-ispell-languages)

(require 'epa-file)

; Interface customizations

;; Hide the bloody bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Make it semi-pretty
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dovrefjell t)
(set-face-attribute 'default nil :font "Dejavu Sans Mono:pixelsize=12")

;; All-important <RET> keyboard shortcut
(define-key evil-normal-state-map (kbd "<RET>") 'save-buffer)

;; Escape quits everything (just like in Vim)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Use auto-fill to 80 in text buffers
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Don't use tabs for indentation (maybe limit this to Haskell?)
(setq-default indent-tabs-mode nil)

;; hl-line-mode, but without the annoying underlining or changing of colours.
(global-hl-line-mode 1)
(set-face-background 'highlight "#111111")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight nil)

(global-set-key [f12] '(lambda () (interactive) (load-file "~/.emacs")))

;; Cursor
(blink-cursor-mode 0)
;; None of these work. The only thing that seems to work is starting emacs with
;; -cr
;;;(set-face-background 'cursor "white")
;;;(add-to-list 'default-frame-alist '(cursor-color . "white"))
;;;(set-cursor-color "white")

;; Expand Region
(require 'expand-region)
(evil-leader/set-key "r" 'er/expand-region)

;; ace jump mode
(require 'ace-jump-mode)
(evil-leader/set-key "f" 'ace-jump-mode)

;; Powerline
(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(powerline-default-theme)

;; Minimap (sublime-style)
(require 'minimap)
(evil-leader/set-key "mo" 'minimap-create)
(evil-leader/set-key "mc" 'minimap-kill)

;; Flymake
(add-to-list 'load-path "~/.emacs.d/emacs-flymake")
(require 'flymake)

; Language: TeX, LaTeX
(defun latex-hook ()
  (reftex-mode)
  (turn-on-auto-fill)
  (TeX-PDF-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (evil-leader/set-key "c" 'reftex-citation))

(add-hook 'LaTeX-mode-hook 'latex-hook)

; This is a nasty hack around the blank window problem with TeX-next-error. I
; have no idea how it works.
(defadvice TeX-parse-reset (after make-master-file-default () activate)
  (push (concat (substring (buffer-name) 1 (- (length (buffer-name)) 8)) "." TeX-default-extension) TeX-error-file)
  (push nil TeX-error-offset))

; Language: R
(require 'ess-site)
(setq ess-swv-processing-command "%s(%s, envir=globalenv())")
(add-hook 'inferior-ess-mode-hook
          '(lambda nil
             (define-key inferior-ess-mode-map (kbd "C-p")
               'comint-previous-matching-input-from-input)
             (define-key inferior-ess-mode-map (kbd "C-n")
               'comint-next-matching-input-from-input)))

; Language: Haskell
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")
(setq haskell-program-name "/home/aleks/local/haskell/bin/ghci")
(add-to-list 'exec-path "~/.cabal/bin/")

(defun haskell-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  (evil-leader/set-key "ht" 'haskell-process-do-type)
  (evil-leader/set-key "hi" 'haskell-process-do-info)
  (ghc-init)
  (flymake-mode)
  (setq ghc-hlint-options '("--ignore=Use camelCase")))

(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(defun haskell-interactive-hook ()
  (define-key haskell-interactive-mode-map (kbd "C-p")
    '(lambda () (interactive) (haskell-interactive-mode-history-toggle 1)))
  (define-key haskell-interactive-mode-map (kbd "C-p")
    '(lambda () (interactive) (haskell-interactive-mode-history-toggle -1))))

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
(add-hook 'haskell-interactive-mode-hook 'haskell-interactive-hook)

;; ghc-mod
(add-to-list 'load-path "~/.emacs.d/ghc-mod")
(autoload 'ghc-init "ghc" nil t)

; Language: Markdown/Pandoc
(require 'pandoc-mode)
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

; Language: Orgmode
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")

;; TODO & agenda

(require 'org-habit)
(evil-leader/set-key "oa" 'org-agenda)
(setq org-log-done 'time)
(setq org-clock-persist 'history)
(setq org-habit-show-habits-only-for-today nil)
(org-clock-persistence-insinuate)

;; Orgmode & RefTeX

(defun org-mode-reftex-search ()
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (reftex-citation t))))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?P . "[[papers:%l][%2a (%y)]]")
	    (?h . "*** %2a (%y) \"%t\"\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-link-abbrev-alist
      '(("bib" . "~/doc/lib/bib/main.bib::%s")
	("notes" . "~/doc/lib/bib/notes.org::#%s")
	("papers" . "~/doc/lib/%s.pdf")))

; Snippets
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

; Magit
(require 'magit)
(evil-leader/set-key "gs" 'magit-status)

(require 'undo-tree)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" default)))
 '(haskell-notify-p t)
 '(haskell-process-type (quote cabal-dev))
 '(haskell-tags-on-save t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/doc/org/work.org" "~/doc/uni/lrs/tp1/lrs.org" "~/doc/org/main.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
