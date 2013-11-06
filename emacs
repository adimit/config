; -*- mode: Emacs-Lisp; -*-

; package
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; set some global variables
(setq
 ; don't show startup screen
 inhibit-startup-message t
 ; paste at cursor, not at mouse pointer position
 mouse-yank-at-point t
 ; vim does this automatically…
 require-final-newline t
 ; don't ask before following symlinks
 vc-follow-symlinks t)

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
(evil-leader/set-key "n" 'server-edit)

; whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing tabs tab-mark))
(whitespace-mode)

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

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactive-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

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

(defface mode-line-file-face
  '()
  "The face used to display the currently opened file in the modeline")
(defface mode-line-mode-face
  '()
  "The face used to display the currently active mode in the modeline")
(defface mode-line-position-face
  '()
  "The face used to display the current position in the modeline")

(defun shorten-directory (path max-length)
  "Show up to `max-length' characters of a path `path'."
  (let ((path (reverse (split-string (abbreviate-file-name path) "/")))
        (output ""))
    ; chop off head of path list if it's empty
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    ; first segment is the file; show it, and delete it from the segment list.
    (setq output (car path))
    (setq path (cdr path))
    ; while we still have space, conjoin the path segments, and shorten the path list
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    ; after we've ran out of space, if we haven't run out of segments, display placeholder
    (when path
      (setq output (concat ".../" output)))
    output))

;; mode line
(setq aleks-mode-line-position
      (list
      (propertize " (%l-%2c) " 'face 'mode-line-position-face)))
(setq aleks-mode-line-buffer-name
      (list
       (propertize " " 'face 'mode-line-file-face)
       (propertize (eval (shorten-directory (concat default-directory "%b") 30)) 'face 'mode-line-file-face)
       (propertize "%+ (%p) " 'face 'mode-line-file-face)))
(setq aleks-mode-line-mode
      (propertize " %m " 'face 'mode-line-mode-face))

(setq-default
 mode-line-format
 (list
   aleks-mode-line-buffer-name
   aleks-mode-line-mode
   aleks-mode-line-position))

;; Expand Region
(require 'expand-region)
(evil-leader/set-key "r" 'er/expand-region)

;; ace jump mode
(require 'ace-jump-mode)
(evil-leader/set-key "f" 'ace-jump-mode)

;; Minimap (sublime-style)
(require 'minimap)
(evil-leader/set-key "mm" 'minimap-create)
(evil-leader/set-key "mc" 'minimap-kill)

;; Flymake
(add-to-list 'load-path "~/.emacs.d/emacs-flymake")
(require 'flymake)

; Language: Java
(defun java-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(add-hook 'java-mode-hook 'java-hook)

; Language: TeX, LaTeX
(defun latex-hook ()
  (reftex-mode)
  (turn-on-auto-fill)
  (TeX-PDF-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (evil-leader/set-key "c" 'reftex-citation))

(add-hook 'LaTeX-mode-hook 'latex-hook)

; We enable -shell-escape so that minted can be used. This is a security risk, though
(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
           '("%`%l%(mode) -shell-escape%' %t"
             TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
           )
  )

; This is a nasty hack around the blank window problem with TeX-next-error. I
; have no idea how it works.
(defadvice TeX-parse-reset (after make-master-file-default () activate)
  (push (concat (substring (buffer-name) 1 (- (length (buffer-name)) 8)) "." TeX-default-extension) TeX-error-file)
  (push nil TeX-error-offset))

; Language: R
(require 'ess-site)
(add-hook 'inferior-ess-mode-hook
          '(lambda nil
             (define-key inferior-ess-mode-map (kbd "C-p")
               'comint-previous-matching-input-from-input)
             (define-key inferior-ess-mode-map (kbd "C-n")
               'comint-next-matching-input-from-input)))

; Language: Haskell
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode-autoloads)
(require 'haskell-unicode-input-method)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")
(setq haskell-program-name "/home/aleks/local/haskell/bin/ghci")
(add-to-list 'exec-path "~/.cabal/bin/")

(defun haskell-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (set-input-method "haskell-unicode")
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

; ; Multiple cursors
; (add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
; (require 'multiple-cursors)
; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

; Magit
(require 'magit)
(evil-leader/set-key "gs" 'magit-status)

; Git-gutter

(global-git-gutter-mode t)

(require 'undo-tree)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-swv-pdflatex-commands (quote ("pdflatex" "texi2pdf" "make")))
 '(ess-swv-processor (quote knitr))
 '(haskell-process-type (quote cabal-repl))
 '(inhibit-startup-screen t))
