(make-face 'font-lock-comment-delimiter-face)
(make-face 'lazy-highlight)

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "cyan")
(set-face-attribute 'font-lock-comment-delimiter-face nil
                    :foreground "cyan")
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "green")
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "blue")
(set-face-attribute 'font-lock-keyword-face nil
                    :bold t
                    :foreground "magenta")
(set-face-attribute 'font-lock-string-face nil
                    :foreground "yellow")
(set-face-attribute 'font-lock-type-face nil
                    :bold t
                    :foreground "green")
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "magenta")
(set-face-attribute 'font-lock-warning-face nil
                    :bold t
                    :foreground "red")

(set-face-attribute 'highlight nil
                    :foreground "blue")
(set-face-attribute 'isearch nil
                    :foreground "white"
                    :background "magenta")
(set-face-attribute 'lazy-highlight nil
                    :bold nil
                    :foreground "black"
                    :background "green")
(set-face-attribute 'region nil
                    :foreground "white"
                    :background "blue")
(set-face-attribute 'trailing-whitespace nil
                    :background "red")

;;;============================================================================
;;; YASNIPPET FACES

(when (featurep 'yasnippet)
  (set-face-attribute 'yas/field-highlight-face nil
                      :foreground "white" :background "magenta")
  (set-face-attribute 'yas/mirror-highlight-face nil
                      :foreground "yellow" :background "magenta"))

(when (featurep 'dropdown-list)
  (set-face-attribute 'dropdown-list-face nil
                      :foreground "black" :background "white")
  (set-face-attribute 'dropdown-list-selection-face nil
                      :foreground "white" :background "blue"))

;;;============================================================================
;;; SHOW-PAREN-MODE FACES

(eval-after-load
 "paren"
 '(progn
    (set-face-attribute 'show-paren-match-face nil
                        :foreground "white" :background "blue")
    (set-face-attribute 'show-paren-mismatch-face nil
                        :foreground "white" :background "red")))

;;;============================================================================
;;; EDIFF FACES

(eval-after-load
    "ediff"
  '(progn
     (set-face-attribute 'ediff-current-diff-A nil
                         :foreground "black" :background "yellow")
     (set-face-attribute 'ediff-current-diff-B nil
                         :foreground "black" :background "yellow")
     (set-face-attribute 'ediff-current-diff-C nil
                         :foreground "black" :background "yellow")
     (set-face-attribute 'ediff-current-diff-Ancestor nil
                         :foreground "black" :background "yellow")
     (set-face-attribute 'ediff-fine-diff-A nil
                         :foreground "black" :background "magenta")
     (set-face-attribute 'ediff-fine-diff-B nil
                         :foreground "black" :background "magenta")
     (set-face-attribute 'ediff-fine-diff-C nil
                         :foreground "black" :background "magenta")
     (set-face-attribute 'ediff-fine-diff-Ancestor nil
                         :foreground "black" :background "magenta")
     (set-face-attribute 'ediff-even-diff-A nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-even-diff-B nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-even-diff-C nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-even-diff-Ancestor nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-odd-diff-A nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-odd-diff-B nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-odd-diff-C nil
                         :foreground "black" :background "cyan")
     (set-face-attribute 'ediff-odd-diff-Ancestor nil
                         :foreground "black" :background "cyan")))