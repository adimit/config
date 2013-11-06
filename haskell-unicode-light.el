;;; haskell-unicode-light.el --- Haskell Unicode helper functions  -*- coding: utf-8 -*-

;;; Adopted from Roel van Dijk's haskell-unicode-input-method.el

(require 'quail)

;;;###autoload

(defun turn-on-haskell-unicode-light-input-method ()
  "Set input method `haskell-unicode-light'."
  (interactive)
  (set-input-method "haskell-unicode-light"))

(quail-define-package
 "haskell-unicode-light" ;; name
 "UTF-8"           ;; language
 "\\"              ;; title
 t                 ;; guidance
 "Haskell Unicode input method.
Designed to be used with the Haskell UnicodeSyntax language extension).
"                  ;; docstring
  nil              ;; translation-keys
  nil              ;; forget-last-selection
  nil              ;; deterministic
  nil              ;; kbd-translate
  nil              ;; show-layout
  nil              ;; create-decode-map
  nil              ;; maximum-shortest
  nil              ;; overlay-plist
  nil              ;; update-translation-function
  nil              ;; conversion-keys
  t                ;; simple
  )

(quail-define-rules
 ;; Greek letters
 ("alpha "           ["α"])
 ("Alpha "           ["Α"])
 ("beta "            ["β"])
 ("Beta "            ["Β"])
 ("gamma "           ["γ"])
 ("Gamma "           ["Γ"])
 ("delta "           ["δ"])
 ("Delta "           ["Δ"])
 ("epsilon "         ["ε"])
 ("Epsilon "         ["Ε"])
 ("zeta "            ["ζ"])
 ("Zeta "            ["Ζ"])
 ("eta "             ["η"])
 ("Eta "             ["Η"])
 ("theta "           ["θ"])
 ("Theta "           ["Θ"])
 ("iota "            ["ι"])
 ("Iota "            ["Ι"])
 ("kappa "           ["κ"])
 ("Kappa "           ["Κ"])
 ("lambda "          ["λ"])
 ("Lambda "          ["Λ"])
 ("lamda "           ["λ"])
 ("Lamda "           ["Λ"])
 ("mu "              ["μ"])
 ("Mu "              ["Μ"])
 ("nu "              ["ν"])
 ("Nu "              ["Ν"])
 ("xi "              ["ξ"])
 ("Xi "              ["Ξ"])
 ("omicron "         ["ο"])
 ("Omicron "         ["Ο"])
 ("pi "              ["π"])
 ("Pi "              ["Π"])
 ("rho "             ["ρ"])
 ("Rho "             ["Ρ"])
 ("sigma "           ["σ"])
 ("Sigma "           ["Σ"])
 ("tau "             ["τ"])
 ("Tau "             ["Τ"])
 ("upsilon "         ["υ"])
 ("Upsilon "         ["Υ"])
 ("phi "             ["φ"])
 ("Phi "             ["Φ"])
 ("chi "             ["χ"])
 ("Chi "             ["Χ"])
 ("psi "             ["ψ"])
 ("Psi "             ["Ψ"])
 ("omega "           ["ω"])
 ("Omega "           ["Ω"])

  ;; Types
 ("::"               ["∷"])

 ;; Quantifiers
 ("forall"           ["∀"])
 ("exists"           ["∃"])

 ;; Arrows
 ("->"               ["→"])
 ("<-"               ["←"])
 ("=>"               ["⇒"]))

(provide 'haskell-unicode-light)

;;; haskell-unicode-light.el ends here
