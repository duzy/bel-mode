;;; bel-mode.el --- Major mode for the Bitcoin Extending Language

;;; Commentary:

;; Copyright 2013 The bel-mode Authors.  All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Author: The bel-mode Authors
;; Version: 1.5.0
;; Keywords: languages bel
;; URL: https://github.com/dominikh/bel-mode.el
;;
;; This file is not part of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'etags)
(require 'ffap)
(require 'find-file)
(require 'ring)
(require 'url)
(require 'xref nil :noerror)  ; xref is new in Emacs 25.1


(eval-when-compile
  (defmacro bel--forward-word (&optional arg)
   (if (fboundp 'forward-word-strictly)
       `(forward-word-strictly ,arg)
     `(forward-word ,arg))))

(defun bel--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun bel-goto-opening-parenthesis (&optional _legacy-unused)
  "Move up one level of parentheses."
  ;; The old implementation of bel-goto-opening-parenthesis had an
  ;; optional argument to speed up the function.  It didn't change the
  ;; function's outcome.

  ;; Silently fail if there's no matching opening parenthesis.
  (condition-case nil
      (backward-up-list)
    (scan-error nil)))


(defconst bel-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst bel--max-dangling-operator-length 2
  "The maximum length of dangling operators.
This must be at least the length of the longest string matched by
‘bel-dangling-operators-regexp.’, and must be updated whenever
that constant is changed.")

(defconst bel-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst bel-type-name-no-prefix-regexp "\\(?:[[:word:][:multibyte:]]+\\.\\)?[[:word:][:multibyte:]]+")
(defconst bel-qualified-identifier-regexp (concat bel-identifier-regexp "\\." bel-identifier-regexp))
(defconst bel-label-regexp bel-identifier-regexp)
(defconst bel-type-regexp "[[:word:][:multibyte:]*]+")
(defconst bel-func-regexp (concat "\\_<func\\_>\\s *\\(" bel-identifier-regexp "\\)"))
(defconst bel-func-meth-regexp (concat
                               "\\_<func\\_>\\s *\\(?:(\\s *"
                               "\\(" bel-identifier-regexp "\\s +\\)?" bel-type-regexp
                               "\\s *)\\s *\\)?\\("
                               bel-identifier-regexp
                               "\\)("))

(defconst bel-builtins
  '("append" "delete" "reduce" "assemble" "checksig"
    "console")
  "All built-in functions in the Bitcoin Extending Language.  Used for font locking.")

(defconst bel-mode-keywords
  '("type" "event" "oracle" "extends" "use" "const" "var" "memory" "modifier"
    "on" "if" "else" "for" "break" "goto" "continue" "return" "assert")
  "All keywords in the Bitcoin Extending Language.  Used for font locking.")
(defconst bel-mode-heading-keywords
  '("pragma" "blockchain" "block" "transaction" "expose")
  "All keywords in the Bitcoin Extending Language.  Used for font locking.")

(defconst bel-constants '("nil" "true" "false"))
(defconst bel-type-name-regexp (concat "\\(?:[*(]\\)*\\(\\(?:" bel-identifier-regexp "\\.\\)?" bel-identifier-regexp "\\)"))

;; Maximum number of identifiers that can be highlighted as type names
;; in one function type/declaration.
(defconst bel--font-lock-func-param-num-groups 16)

(defvar bel-dangling-cache)

(defgroup bel nil
  "Major mode for editing BEL code."
  :link '(url-link "https://github.com/dominikh/bel-mode.el")
  :group 'languages)

(defgroup bel-cover nil
  "Options specific to `cover`."
  :group 'bel)

(defcustom bel-fontify-function-calls t
  "Fontify function and method calls if this is non-nil."
  :type 'boolean
  :group 'bel)

(defcustom bel-mode-hook nil
  "Hook called by `bel-mode'."
  :type 'hook
  :group 'bel)

(defcustom bel-command "bel"
  "The 'bel' command.
Some users have multiple Bel development trees and invoke the 'go'
tool via a wrapper that sets GOROOT and GOPATH based on the
current directory.  Such users should customize this variable to
point to the wrapper script."
  :type 'string
  :group 'bel)

(defcustom bel-other-file-alist
  '(("_test\\.bel\\'" (".bel"))
    ("\\.bel\\'" ("_test.bel")))
  "See the documentation of `ff-other-file-alist' for details."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :group 'bel)

(defcustom bel-packages-function 'bel-packages-native
  "Function called by `bel-packages' to determine the list of available packages.
This is used in e.g. tab completion in `bel-import-add'.

This package provides two functions: `bel-packages-native' uses
elisp to find all .a files in all /pkg/ directories.
`bel-packages-bel-list' uses 'go list all' to determine all Go
packages.  `bel-packages-bel-list' generally produces more accurate
results, but can be slower than `bel-packages-native'."
  :type 'function
  :package-version '(bel-mode . 1.4.0)
  :group 'bel)

(defcustom bel-guess-gopath-functions (list #'bel-godep-gopath
                                           #'bel-wbel-gopath
                                           #'bel-gb-gopath
                                           #'bel-plain-gopath)
  "Functions to call in sequence to detect a project's GOPATH.

The functions in this list will be called one after another,
until a function returns non-nil.  The order of the functions in
this list is important, as some project layouts may superficially
look like others.  For example, a subset of wgo projects look like
gb projects.  That's why we need to detect wgo first, to avoid
mis-identifying them as gb projects."
  :type '(repeat function)
  :group 'bel)

(defun bel--kill-new-message (url)
  "Make URL the latest kill and print a message."
  (kill-new url)
  (message "%s" url))

(defcustom bel-play-browse-function 'bel--kill-new-message
  "Function to call with the Playground URL.
See `bel-play-region' for more details."
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Kill + Message" bel--kill-new-message)
          (const :tag "Browse URL" browse-url)
          (function :tag "Call function"))
  :group 'bel)

(defvar bel-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?^  "." st)
    (modify-syntax-entry ?!  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?`  "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; TODO make _ a symbol constituent now that xemacs is gone
    (modify-syntax-entry ?_  "w" st)

    st)
  "Syntax table for Go mode.")

(defun bel--build-font-lock-keywords ()
  ;; we cannot use 'symbols in regexp-opt because GNU Emacs <24
  ;; doesn't understand that
  (append
   `((bel--match-func
      ,@(mapcar (lambda (x) `(,x font-lock-type-face))
                (number-sequence 1 bel--font-lock-func-param-num-groups)))
     (,(concat "^\\_<" (regexp-opt bel-mode-heading-keywords t) "\\_>") . font-lock-keyword-face)
     (,(concat "\\_<" (regexp-opt bel-mode-keywords t) "\\_>") . font-lock-keyword-face)
     (,(concat "\\(\\_<" (regexp-opt bel-builtins t) "\\_>\\)[[:space:]]*(") 1 font-lock-builtin-face)
     (,(concat "\\_<" (regexp-opt bel-constants t) "\\_>") . font-lock-constant-face)
     (,bel-func-regexp 1 font-lock-function-name-face)) ;; function (not method) name

   (if bel-fontify-function-calls
       `((,(concat "\\(" bel-identifier-regexp "\\)[[:space:]]*(") 1 font-lock-function-name-face) ;; function call/method name
         (,(concat "[^[:word:][:multibyte:]](\\(" bel-identifier-regexp "\\))[[:space:]]*(") 1 font-lock-function-name-face)) ;; bracketed function call
     `((,bel-func-meth-regexp 2 font-lock-function-name-face))) ;; method name

   `(
     ("\\(`[^`]*`\\)" 1 font-lock-multiline) ;; raw string literal, needed for font-lock-syntactic-keywords
     (,(concat "\\_<type\\_>[[:space:]]+\\([^[:space:](]+\\)") 1 font-lock-type-face) ;; types
     (,(concat "\\_<type\\_>[[:space:]]+" bel-identifier-regexp "[[:space:]]*" bel-type-name-regexp) 1 font-lock-type-face) ;; types
     (,(concat "[^[:word:][:multibyte:]]\\[\\([[:digit:]]+\\|\\.\\.\\.\\)?\\]" bel-type-name-regexp) 2 font-lock-type-face) ;; Arrays/slices
     (,(concat "\\(" bel-identifier-regexp "\\)" "{") 1 font-lock-type-face)
     (,(concat "\\_<map\\_>\\[[^]]+\\]" bel-type-name-regexp) 1 font-lock-type-face) ;; map value type
     (,(concat "\\_<map\\_>\\[" bel-type-name-regexp) 1 font-lock-type-face) ;; map key type
     (,(concat "\\_<chan\\_>[[:space:]]*\\(?:<-[[:space:]]*\\)?" bel-type-name-regexp) 1 font-lock-type-face) ;; channel type
     (,(concat "\\_<\\(?:new\\|make\\)\\_>\\(?:[[:space:]]\\|)\\)*(" bel-type-name-regexp) 1 font-lock-type-face) ;; new/make type
     ;; TODO do we actually need this one or isn't it just a function call?
     (,(concat "\\.\\s *(" bel-type-name-regexp) 1 font-lock-type-face) ;; Type conversion
     ;; Like the original bel-mode this also marks compound literal
     ;; fields. There, it was marked as to fix, but I grew quite
     ;; accustomed to it, so it'll stay for now.
     (,(concat "^[[:space:]]*\\(" bel-label-regexp "\\)[[:space:]]*:\\(\\S.\\|$\\)") 1 font-lock-constant-face) ;; Labels and compound literal fields
     (,(concat "\\_<\\(goto\\|break\\|continue\\)\\_>[[:space:]]*\\(" bel-label-regexp "\\)") 2 font-lock-constant-face)))) ;; labels in goto/break/continue

(let ((m (define-prefix-command 'bel-goto-map)))
  (define-key m "a" #'bel-goto-arguments)
  (define-key m "d" #'bel-goto-docstring)
  (define-key m "f" #'bel-goto-function)
  (define-key m "i" #'bel-goto-imports)
  (define-key m "m" #'bel-goto-method-receiver)
  (define-key m "n" #'bel-goto-function-name)
  (define-key m "r" #'bel-goto-return-values))

(defvar bel-mode-map
  (let ((m (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)
        (define-key m "}" #'bel-mode-insert-and-indent)
        (define-key m ")" #'bel-mode-insert-and-indent))
    (define-key m (kbd "C-c C-a") #'bel-import-add)
    ;;(define-key m (kbd "C-c C-j") #'godef-jump)
    ;;(define-key m (kbd "C-x 4 C-c C-j") #'godef-jump-other-window)
    ;;(define-key m (kbd "C-c C-d") #'godef-describe)
    (define-key m (kbd "C-c C-f") 'bel-goto-map)
    m)
  "Keymap used by ‘bel-mode’.")

(easy-menu-define bel-mode-menu bel-mode-map
  "Menu for BEL mode."
  '("BEL"
    ["Add Import"            bel-import-add t]
    ["Remove Unused Imports" bel-remove-unused-imports t]
    ["Go to Imports"         bel-goto-imports t]
    "---"
    ("Playground"
     ["Send Buffer"          bel-play-buffer t]
     ["Send Region"          bel-play-region t]
     ["Download"             bel-download-play t])
    "---"
    ["Customize Mode"        (customize-group 'bel) t]))

(defun bel-mode-insert-and-indent (key)
  "Invoke the global binding of KEY, then reindent the line."

  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))

(defmacro bel-paren-level ()
  `(car (syntax-ppss)))

(defmacro bel-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

(defmacro bel-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro bel-in-comment-p ()
  `(nth 4 (syntax-ppss)))

(defmacro bel-goto-beginning-of-string-or-comment ()
  `(goto-char (nth 8 (syntax-ppss))))

(defun bel--backward-irrelevant (&optional stop-at-string)
  "Skip backwards over any characters that are irrelevant for
indentation and related tasks.

It skips over whitespace, comments, cases and labels and, if
STOP-AT-STRING is not true, over strings."

  (let (pos (start-pos (point)))
    (skip-chars-backward "\n\s\t")
    (if (and (save-excursion (beginning-of-line) (bel-in-string-p))
             (= (char-before) ?`)
             (not stop-at-string))
        (backward-char))
    (if (and (bel-in-string-p)
             (not stop-at-string))
        (bel-goto-beginning-of-string-or-comment))
    (if (looking-back "\\*/" (line-beginning-position))
        (backward-char))
    (if (bel-in-comment-p)
        (bel-goto-beginning-of-string-or-comment))
    (setq pos (point))
    (beginning-of-line)
    (if (or (looking-at (concat "^" bel-label-regexp ":"))
            (looking-at "^[[:space:]]*\\(case .+\\|default\\):"))
        (end-of-line 0)
      (goto-char pos))
    (if (/= start-pos (point))
        (bel--backward-irrelevant stop-at-string))
    (/= start-pos (point))))

(defun bel--buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (buffer-size)
      (- (point-max)
         (point-min))))

(defun bel-previous-line-has-dangling-op-p ()
  "Return non-nil if the current line is a continuation line."
  (let* ((cur-line (line-number-at-pos))
         (val (gethash cur-line bel-dangling-cache 'nope)))
    (if (or (bel--buffer-narrowed-p) (equal val 'nope))
        (save-excursion
          (beginning-of-line)
          (bel--backward-irrelevant t)
          (setq val (looking-back bel-dangling-operators-regexp
                                  (- (point) bel--max-dangling-operator-length)))
          (if (not (bel--buffer-narrowed-p))
              (puthash cur-line val bel-dangling-cache))))
    val))

(defun bel--at-function-definition ()
  "Return non-nil if point is on the opening curly brace of a
function definition.

We do this by first calling (beginning-of-defun), which will take
us to the start of *some* function.  We then look for the opening
curly brace of that function and compare its position against the
curly brace we are checking.  If they match, we return non-nil."
  (if (= (char-after) ?\{)
      (save-excursion
        (let ((old-point (point))
              start-nesting)
          (beginning-of-defun)
          (when (looking-at "func ")
            (setq start-nesting (bel-paren-level))
            (skip-chars-forward "^{")
            (while (> (bel-paren-level) start-nesting)
              (forward-char)
              (skip-chars-forward "^{") 0)
            (if (and (= (bel-paren-level) start-nesting) (= old-point (point)))
                t))))))

(defun bel--indentation-for-opening-parenthesis ()
  "Return the semantic indentation for the current opening parenthesis.

If point is on an opening curly brace and said curly brace
belongs to a function declaration, the indentation of the func
keyword will be returned.  Otherwise the indentation of the
current line will be returned."
  (save-excursion
    (if (bel--at-function-definition)
        (progn
          (beginning-of-defun)
          (current-indentation))
      (current-indentation))))

(defun bel-indentation-at-point ()
  (save-excursion
    (let (start-nesting)
      (back-to-indentation)
      (setq start-nesting (bel-paren-level))

      (cond
       ((bel-in-string-p)
        (current-indentation))
       ((looking-at "[])}]")
        (bel-goto-opening-parenthesis)
        (if (bel-previous-line-has-dangling-op-p)
            (- (current-indentation) tab-width)
          (bel--indentation-for-opening-parenthesis)))
       ((progn (bel--backward-irrelevant t)
               (looking-back bel-dangling-operators-regexp
                             (- (point) bel--max-dangling-operator-length)))
        ;; only one nesting for all dangling operators in one operation
        (if (bel-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (current-indentation) tab-width)))
       ((zerop (bel-paren-level))
        0)
       ((progn (bel-goto-opening-parenthesis) (< (bel-paren-level) start-nesting))
        (if (bel-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (bel--indentation-for-opening-parenthesis) tab-width)))
       (t
        (current-indentation))))))

(defun bel-mode-indent-line ()
  (interactive)
  (let (indent
        shift-amt
        (pos (- (point-max) (point)))
        (point (point))
        (beg (line-beginning-position)))
    (back-to-indentation)
    (if (bel-in-string-or-comment-p)
        (goto-char point)
      (setq indent (bel-indentation-at-point))
      (if (looking-at (concat bel-label-regexp ":\\([[:space:]]*/.+\\)?$\\|case .+:\\|default:"))
          (cl-decf indent tab-width))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun bel-beginning-of-defun (&optional count)
  (unless (bolp)
    (end-of-line))
  (setq count (or count 1))
  (let (first failure)
    (dotimes (i (abs count))
      (setq first t)
      (while (and (not failure)
                  (or first (bel-in-string-or-comment-p)))
        (if (>= count 0)
            (progn
              (bel--backward-irrelevant)
              (if (not (re-search-backward bel-func-meth-regexp nil t))
                  (setq failure t)))
          (if (looking-at bel-func-meth-regexp)
              (forward-char))
          (if (not (re-search-forward bel-func-meth-regexp nil t))
              (setq failure t)))
        (setq first nil)))
    (if (< count 0)
        (beginning-of-line))
    (not failure)))

(defun bel-end-of-defun ()
  (let (orig-level)
    ;; It can happen that we're not placed before a function by emacs
    (if (not (looking-at "func"))
        (bel-beginning-of-defun -1))
    ;; Find the { that starts the function, i.e., the next { that isn't
    ;; preceded by struct or interface, or a comment or struct tag.  BUG:
    ;; breaks if there's a comment between the struct/interface keyword and
    ;; bracket, like this:
    ;;
    ;;     struct /* why? */ {
    (while (progn
      (skip-chars-forward "^{")
      (forward-char)
      (or (bel-in-string-or-comment-p)
          (looking-back "\\(struct\\|interface\\)\\s-*{"
                        (line-beginning-position)))))
    (setq orig-level (bel-paren-level))
    (while (>= (bel-paren-level) orig-level)
      (skip-chars-forward "^}")
      (forward-char))))

(defun bel--find-enclosing-parentheses (position)
  "Return points of outermost '(' and ')' surrounding POSITION if
such parentheses exist.

If outermost '(' exists but ')' does not, it returns the next blank
line or end-of-buffer position instead of the position of the closing
parenthesis.

If the starting parenthesis is not found, it returns (POSITION
POSITION)."
  (save-excursion
    (let (beg end)
      (goto-char position)
      (while (> (bel-paren-level) 0)
        (re-search-backward "[(\\[{]" nil t)
        (when (looking-at "(")
          (setq beg (point))))
      (if (null beg)
          (list position position)
        (goto-char position)
        (while (and (> (bel-paren-level) 0)
                    (search-forward ")" nil t)))
        (when (> (bel-paren-level) 0)
          (unless (re-search-forward "^[[:space:]]*$" nil t)
            (goto-char (point-max))))
        (list beg (point))))))

(defun bel--search-next-comma (end)
  "Search forward from point for a comma whose nesting level is
the same as point.  If it reaches the end of line or a closing
parenthesis before a comma, it stops at it."
  (let ((orig-level (bel-paren-level)))
    (while (and (< (point) end)
                (or (looking-at "[^,)\n]")
                    (> (bel-paren-level) orig-level)))
      (forward-char))
    (when (and (looking-at ",")
               (< (point) (1- end)))
      (forward-char))))

(defun bel--looking-at-keyword ()
  (and (looking-at (concat "\\(" bel-identifier-regexp "\\)"))
       (member (match-string 1) bel-mode-keywords)))

(defun bel--match-func (end)
  "Search for identifiers used as type names from a function
parameter list, and set the identifier positions as the results
of last search.  Return t if search succeeded."
  (when (re-search-forward "\\_<func\\_>" end t)
    (let ((regions (bel--match-func-type-names end)))
      (if (null regions)
          ;; Nothing to highlight. This can happen if the current func
          ;; is "func()". Try next one.
          (bel--match-func end)
        ;; There are something to highlight. Set those positions as
        ;; last search results.
        (setq regions (bel--filter-match-data regions end))
        (when regions
          (set-match-data (bel--make-match-data regions))
          t)))))

(defun bel--match-func-type-names (end)
  (cond
   ;; Function declaration (e.g. "func foo(")
   ((looking-at (concat "[[:space:]\n]*" bel-identifier-regexp "[[:space:]\n]*("))
    (goto-char (match-end 0))
    (nconc (bel--match-parameter-list end)
           (bel--match-function-result end)))
   ;; Method declaration, function literal, or function type
   ((looking-at "[[:space:]]*(")
    (goto-char (match-end 0))
    (let ((regions (bel--match-parameter-list end)))
      ;; Method declaration (e.g. "func (x y) foo(")
      (when (looking-at (concat "[[:space:]]*" bel-identifier-regexp "[[:space:]\n]*("))
        (goto-char (match-end 0))
        (setq regions (nconc regions (bel--match-parameter-list end))))
      (nconc regions (bel--match-function-result end))))))

(defun bel--parameter-list-type (end)
  "Return `present' if the parameter list has names, or `absent' if not.
Assumes point is at the beginning of a parameter list, just
after '('."
  (save-excursion
    (skip-chars-forward "[:space:]\n" end)
    (cond ((> (point) end)
           nil)
          ((looking-at (concat bel-identifier-regexp "[[:space:]\n]*,"))
           (goto-char (match-end 0))
           (bel--parameter-list-type end))
          ((or (looking-at bel-qualified-identifier-regexp)
               (looking-at (concat bel-type-name-no-prefix-regexp "[[:space:]\n]*\\(?:)\\|\\'\\)"))
               (bel--looking-at-keyword)
               (looking-at "[*\\[]\\|\\.\\.\\.\\|\\'"))
           'absent)
          (t 'present))))

(defconst bel--opt-dotdotdot-regexp "\\(?:\\.\\.\\.\\)?")
(defconst bel--parameter-type-regexp
  (concat bel--opt-dotdotdot-regexp "[[:space:]*\n]*\\(" bel-type-name-no-prefix-regexp "\\)[[:space:]\n]*\\([,)]\\|\\'\\)"))
(defconst bel--func-type-in-parameter-list-regexp
  (concat bel--opt-dotdotdot-regexp "[[:space:]*\n]*\\(\\_<func\\_>" "\\)"))

(defun bel--match-parameters-common (identifier-regexp end)
  (let ((acc ())
        (start -1))
    (while (progn (skip-chars-forward "[:space:]\n" end)
                  (and (not (looking-at "\\(?:)\\|\\'\\)"))
                       (< start (point))
                       (<= (point) end)))
      (setq start (point))
      (cond
       ((looking-at (concat identifier-regexp bel--parameter-type-regexp))
        (setq acc (nconc acc (list (match-beginning 1) (match-end 1))))
        (goto-char (match-beginning 2)))
       ((looking-at (concat identifier-regexp bel--func-type-in-parameter-list-regexp))
        (goto-char (match-beginning 1))
        (setq acc (nconc acc (bel--match-func-type-names end)))
        (bel--search-next-comma end))
       (t
        (bel--search-next-comma end))))
    (when (and (looking-at ")")
               (< (point) end))
      (forward-char))
    acc))

(defun bel--match-parameters-with-identifier-list (end)
  (bel--match-parameters-common
   (concat bel-identifier-regexp "[[:space:]\n]+")
   end))

(defun bel--match-parameters-without-identifier-list (end)
  (bel--match-parameters-common "" end))

(defun bel--filter-match-data (regions end)
  "Remove points from REGIONS if they are beyond END.
REGIONS are a list whose size is multiple of 2.  Element 2n is beginning of a
region and 2n+1 is end of it.

This function is used to make sure we don't override end point
that `font-lock-mode' gave to us."
  (when regions
    (let* ((vec (vconcat regions))
           (i 0)
           (len (length vec)))
      (while (and (< i len)
                  (<= (nth i regions) end)
                  (<= (nth (1+ i) regions) end))
        (setq i (+ i 2)))
      (cond ((= i len)
             regions)
            ((zerop i)
             nil)
            (t
             (butlast regions (- (length regions) i)))))))

(defun bel--make-match-data (regions)
  (let ((deficit (- (* 2 bel--font-lock-func-param-num-groups)
                    (length regions))))
    (when (> deficit 0)
      (let ((last (car (last regions))))
        (setq regions (nconc regions (make-list deficit last))))))
  `(,(car regions) ,@(last regions) ,@regions))

(defun bel--match-parameter-list (end)
  "Return a list of identifier positions that are used as type
names in a function parameter list, assuming point is at the
beginning of a parameter list.  Return nil if the text after
point does not look like a parameter list.

Set point to end of closing parenthesis on success.

In Go, the names must either all be present or all be absent
within a list of parameters.

Parsing a parameter list is a little bit complicated because we
have to scan through the parameter list to determine whether or
not the list has names. Until a type name is found or reaching
end of a parameter list, we are not sure which form the parameter
list is.

For example, X and Y are type names in a parameter list \"(X,
Y)\" but are parameter names in \"(X, Y int)\". We cannot say if
X is a type name until we see int after Y.

Note that even \"(int, float T)\" is a valid parameter
list. Builtin type names are not reserved words. In this example,
int and float are parameter names and only T is a type name.

In this function, we first scan the parameter list to see if the
list has names, and then handle it accordingly."
  (let ((name (bel--parameter-list-type end)))
    (cond ((eq name 'present)
           (bel--match-parameters-with-identifier-list end))
          ((eq name 'absent)
           (bel--match-parameters-without-identifier-list end))
          (t nil))))

(defun bel--match-function-result (end)
  "Return a list of identifier positions that are used as type
names in a function result, assuming point is at the beginning of
a result.

Function result is a unparenthesized type or a parameter list."
  (cond ((and (looking-at (concat "[[:space:]*]*\\(" bel-type-name-no-prefix-regexp "\\)"))
              (not (member (match-string 1) bel-mode-keywords)))
         (list (match-beginning 1) (match-end 1)))
        ((looking-at "[[:space:]]*(")
         (goto-char (match-end 0))
         (bel--match-parameter-list end))
        (t nil)))

(defun bel--reset-dangling-cache-before-change (&optional _beg _end)
  "Reset `bel-dangling-cache'.

This is intended to be called from `before-change-functions'."
  (setq bel-dangling-cache (make-hash-table :test 'eql)))

;;;###autoload
(define-derived-mode bel-mode prog-mode "Bel"
  "Major mode for editing BEL source text."

  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(bel--build-font-lock-keywords))

  ;; Indentation
  (set (make-local-variable 'indent-line-function) #'bel-mode-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  (set (make-local-variable 'beginning-of-defun-function) #'bel-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'bel-end-of-defun)

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'syntax-propertize-function) #'bel-propertize-syntax)

  (if (boundp 'electric-indent-chars)
      (set (make-local-variable 'electric-indent-chars) '(?\n ?} ?\))))

  (set (make-local-variable 'compilation-error-screen-columns) nil)

  (set (make-local-variable 'bel-dangling-cache) (make-hash-table :test 'eql))
  (add-hook 'before-change-functions #'bel--reset-dangling-cache-before-change t t)

  ;; ff-find-other-file
  (setq ff-other-file-alist 'bel-other-file-alist)

  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  ;; Go style
  (setq indent-tabs-mode t)

  ;; Handle unit test failure output in compilation-mode
  ;;
  ;; Note that we add our entry to the beginning of
  ;; compilation-error-regexp-alist. In older versions of Emacs, the
  ;; list was processed from the end, and we would've wanted to add
  ;; ours last. But at some point this changed, and now the list is
  ;; processed from the beginning. It's important that our entry comes
  ;; before gnu, because gnu matches go test output, but includes the
  ;; leading whitespace in the file name.
  ;;
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2001-12/msg00674.html
  ;; documents the old, reverseed order.
  (when (and (boundp 'compilation-error-regexp-alist)
             (boundp 'compilation-error-regexp-alist-alist))
    (add-to-list 'compilation-error-regexp-alist 'bel-test)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(bel-test . ("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.bel\\'" 'bel-mode))

(defun bel--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in bel--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (bel--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (bel--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in bel--apply-rcs-patch")))))))
    (move-to-column column)))

(defun bel-goto-imports ()
  "Move point to the block of imports.

If using

  import (
    \"foo\"
    \"bar\"
  )

it will move point directly behind the last import.

If using

  import \"foo\"
  import \"bar\"

it will move point to the next line after the last import.

If no imports can be found, point will be moved after the package
declaration."
  (interactive)
  ;; FIXME if there's a block-commented import before the real
  ;; imports, we'll jump to that one.

  ;; Generally, this function isn't very forgiving. it'll bark on
  ;; extra whitespace. It works well for clean code.
  (let ((old-point (point)))
    (goto-char (point-min))
    (cond
     ((re-search-forward "^import ()" nil t)
      (backward-char 1)
      'block-empty)
     ((re-search-forward "^import ([^)]+)" nil t)
      (backward-char 2)
      'block)
     ((re-search-forward "\\(^import \\([^\"]+ \\)?\"[^\"]+\"\n?\\)+" nil t)
      'single)
     ((re-search-forward "^[[:space:]\n]*package .+?\n" nil t)
      (message "No imports found, moving point after package declaration")
      'none)
     (t
      (goto-char old-point)
      (message "No imports or package declaration found. Is this really a Go file?")
      'fail))))

(defun bel-play-buffer ()
  "Like `bel-play-region', but acts on the entire buffer."
  (interactive)
  (bel-play-region (point-min) (point-max)))

(defun bel-play-region (start end)
  "Send the region between START and END to the Playground.
If non-nil `bel-play-browse-function' is called with the
Playground URL."
  (interactive "r")
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (encode-coding-string
           (buffer-substring-no-properties start end)
           'utf-8))
         (content-buf (url-retrieve
                       "https://play.golang.org/share"
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'bel-play-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (let ((url (format "https://play.golang.org/p/%s"
                                              (buffer-substring (point) (point-max)))))
                             (when bel-play-browse-function
                               (funcall bel-play-browse-function url)))))))))))

;;;###autoload
(defun bel-download-play (url)
  "Download a paste from the playground and insert it in a Go buffer.
Tries to look for a URL at point."
  (interactive (list (read-from-minibuffer "Playground URL: " (ffap-url-p (ffap-string-at-point 'url)))))
  (with-current-buffer
      (let ((url-request-method "GET") url-request-data url-request-extra-headers)
        (url-retrieve-synchronously (concat url ".go")))
    (let ((buffer (generate-new-buffer (concat (car (last (split-string url "/"))) ".go"))))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (copy-to-buffer buffer (point) (point-max))
      (kill-buffer)
      (with-current-buffer buffer
        (bel-mode)
        (switch-to-buffer buffer)))))

(defun bel-propertize-syntax (start end)
  (save-excursion
    (goto-char start)
    (while (search-forward "\\" end t)
      (put-text-property (1- (point)) (point) 'syntax-table (if (= (char-after) ?`) '(1) '(9))))))

(defun bel-import-add (arg import)
  "Add a new IMPORT to the list of imports.

When called with a prefix ARG asks for an alternative name to
import the package as.

If no list exists yet, one will be created if possible.

If an identical import has been commented, it will be
uncommented, otherwise a new import will be added."

  ;; - If there's a matching `// import "foo"`, uncomment it
  ;; - If we're in an import() block and there's a matching `"foo"`, uncomment it
  ;; - Otherwise add a new import, with the appropriate syntax
  (interactive
   (list
    current-prefix-arg
    (replace-regexp-in-string "^[\"']\\|[\"']$" "" (completing-read "Package: " (bel-packages)))))
  (save-excursion
    (let (as line import-start)
      (if arg
          (setq as (read-from-minibuffer "Import as: ")))
      (if as
          (setq line (format "%s \"%s\"" as import))
        (setq line (format "\"%s\"" import)))

      (goto-char (point-min))
      (if (re-search-forward (concat "^[[:space:]]*//[[:space:]]*import " line "$") nil t)
          (uncomment-region (line-beginning-position) (line-end-position))
        (cl-case (bel-goto-imports)
          ('fail (message "Could not find a place to add import."))
          ('block-empty
           (insert "\n\t" line "\n"))
          ('block
              (save-excursion
                (re-search-backward "^import (")
                (setq import-start (point)))
            (if (re-search-backward (concat "^[[:space:]]*//[[:space:]]*" line "$")  import-start t)
                (uncomment-region (line-beginning-position) (line-end-position))
              (insert "\n\t" line)))
          ('single (insert "import " line "\n"))
          ('none (insert "\nimport (\n\t" line "\n)\n")))))))

(defun bel-root-and-paths ()
  (let* ((output (process-lines bel-command "env" "GOROOT" "GOPATH"))
         (root (car output))
         (paths (split-string (cadr output) path-separator)))
    (cons root paths)))

(defun bel--string-prefix-p (s1 s2 &optional ignore-case)
  "Return non-nil if S1 is a prefix of S2.
If IGNORE-CASE is non-nil, the comparison is case-insensitive."
  (eq t (compare-strings s1 nil nil
                         s2 0 (length s1) ignore-case)))

(defun bel--directory-dirs (dir)
  "Recursively return all subdirectories in DIR."
  (if (file-directory-p dir)
      (let ((dir (directory-file-name dir))
            (dirs '())
            (files (directory-files dir nil nil t)))
        (dolist (file files)
          (unless (member file '("." ".."))
            (let ((file (concat dir "/" file)))
              (if (file-directory-p file)
                  (setq dirs (append (cons file
                                           (bel--directory-dirs file))
                                     dirs))))))
        dirs)
    '()))


(defun bel-packages ()
  (funcall bel-packages-function))

(defun bel-packages-native ()
  "Return a list of all installed Go packages.
It looks for archive files in /pkg/."
  (sort
   (delete-dups
    (cl-mapcan
     (lambda (topdir)
       (let ((pkgdir (concat topdir "/pkg/")))
         (cl-mapcan (lambda (dir)
                   (mapcar (lambda (file)
                             (let ((sub (substring file (length pkgdir) -2)))
                               (unless (or (bel--string-prefix-p "obj/" sub) (bel--string-prefix-p "tool/" sub))
                                 (mapconcat #'identity (cdr (split-string sub "/")) "/"))))
                           (if (file-directory-p dir)
                               (directory-files dir t "\\.a$"))))
                 (if (file-directory-p pkgdir)
                     (bel--directory-dirs pkgdir)))))
     (bel-root-and-paths)))
   #'string<))

(defun bel-packages-bel-list ()
  "Return a list of all Go packages, using `go list'."
  (process-lines bel-command "list" "-e" "all"))

(defun bel-unused-imports-lines ()
  (reverse (remove nil
                   (mapcar
                    (lambda (line)
                      (when (string-match "^\\(.+\\):\\([[:digit:]]+\\): imported and not used: \".+\".*$" line)
                        (let ((error-file-name (match-string 1 line))
                              (error-line-num (match-string 2 line)))
                          (if (string= (file-truename error-file-name) (file-truename buffer-file-name))
                              (string-to-number error-line-num)))))
                    (split-string (shell-command-to-string
                                   (concat bel-command
                                           (if (string-match "_test\\.go$" buffer-file-truename)
                                               " test -c"
                                             (concat " build -o " null-device))
                                           " -gcflags=-e"
                                           " "
                                           (shell-quote-argument (file-truename buffer-file-name)))) "\n")))))

(defun bel-remove-unused-imports (arg)
  "Remove all unused imports.
If ARG is non-nil, unused imports will be commented, otherwise
they will be removed completely."
  (interactive "P")
  (save-excursion
    (let ((cur-buffer (current-buffer)) flymake-state lines)
      (when (boundp 'flymake-mode)
        (setq flymake-state flymake-mode)
        (flymake-mode-off))
      (save-some-buffers nil (lambda () (equal cur-buffer (current-buffer))))
      (if (buffer-modified-p)
          (message "Cannot operate on unsaved buffer")
        (setq lines (bel-unused-imports-lines))
        (dolist (import lines)
          (bel--goto-line import)
          (beginning-of-line)
          (if arg
              (comment-region (line-beginning-position) (line-end-position))
            (bel--delete-whole-line)))
        (message "Removed %d imports" (length lines)))
      (if flymake-state (flymake-mode-on)))))

(defun bel--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun bel--line-column-to-point (line column)
  (save-excursion
    (bel--goto-line line)
    (forward-char (1- column))
    (point)))

(cl-defstruct bel--covered
  start-line start-column end-line end-column covered count)

(defun bel-goto-function (&optional arg)
  "Go to the function defintion (named or anonymous) surrounding point.

If we are on a docstring, follow the docstring down.
If no function is found, assume that we are at the top of a file
and search forward instead.

If point is looking at the func keyword of an anonymous function,
go to the surrounding function.

If ARG is non-nil, anonymous functions are ignored."
  (interactive "P")
  (let ((p (point)))
    (cond
     ((save-excursion
        (beginning-of-line)
        (looking-at "^//"))
      ;; In case we are looking at the docstring, move on forward until we are
      ;; not anymore
      (beginning-of-line)
      (while (looking-at "^//")
        (forward-line 1))
      ;; If we are still not looking at a function, retry by calling self again.
      (when (not (looking-at "\\<func\\>"))
        (bel-goto-function arg)))

     ;; If we're already looking at an anonymous func, look for the
     ;; surrounding function.
     ((and (looking-at "\\<func\\>")
           (not (looking-at "^func\\>")))
      (re-search-backward "\\<func\\>" nil t))

     ((not (looking-at "\\<func\\>"))
      ;; If point is on the "func" keyword, step back a word and retry
      (if (string= (symbol-name (symbol-at-point)) "func")
          (backward-word)
        ;; If we are not looking at the beginning of a function line, do a regexp
        ;; search backwards
        (re-search-backward "\\<func\\>" nil t))

      ;; If nothing is found, assume that we are at the top of the file and
      ;; should search forward instead.
      (when (not (looking-at "\\<func\\>"))
        (re-search-forward "\\<func\\>" nil t)
        (bel--forward-word -1))

      ;; If we have landed at an anonymous function, it is possible that we
      ;; were not inside it but below it. If we were not inside it, we should
      ;; go to the containing function.
      (while (and (not (bel--in-function-p p))
                  (not (looking-at "^func\\>")))
        (bel-goto-function arg)))))

  (cond
   ((bel-in-comment-p)
    ;; If we are still in a comment, redo the call so that we get out of it.
    (bel-goto-function arg))

   ((and (looking-at "\\<func(") arg)
    ;; If we are looking at an anonymous function and a prefix argument has
    ;; been supplied, redo the call so that we skip the anonymous function.
    (bel-goto-function arg))))

(defun bel--goto-opening-curly-brace ()
  ;; Find the { that starts the function, i.e., the next { that isn't
  ;; preceded by struct or interface, or a comment or struct tag.  BUG:
  ;; breaks if there's a comment between the struct/interface keyword and
  ;; bracket, like this:
  ;;
  ;;     struct /* why? */ {
  (bel--goto-return-values)
  (while (progn
           (skip-chars-forward "^{")
           (forward-char)
           (or (bel-in-string-or-comment-p)
               (looking-back "\\(struct\\|interface\\)\\s-*{"
                             (line-beginning-position)))))
  (backward-char))

(defun bel--in-function-p (compare-point)
  "Return t if COMPARE-POINT lies inside the function immediately surrounding point."
  (save-excursion
    (when (not (looking-at "\\<func\\>"))
      (bel-goto-function))
    (let ((start (point)))
      (bel--goto-opening-curly-brace)

      (unless (looking-at "{")
        (error "Expected to be looking at opening curly brace"))
      (forward-list 1)
      (and (>= compare-point start)
           (<= compare-point (point))))))

(defun bel-goto-function-name (&optional arg)
  "Go to the name of the current function.

If the function is a test, place point after 'Test'.
If the function is anonymous, place point on the 'func' keyword.

If ARG is non-nil, anonymous functions are skipped."
  (interactive "P")
  (when (not (looking-at "\\<func\\>"))
    (bel-goto-function arg))
  ;; If we are looking at func( we are on an anonymous function and
  ;; nothing else should be done.
  (when (not (looking-at "\\<func("))
    (let ((words 1)
          (chars 1))
      (when (looking-at "\\<func (")
        (setq words 3
              chars 2))
      (bel--forward-word words)
      (forward-char chars)
      (when (looking-at "Test")
        (forward-char 4)))))

(defun bel-goto-arguments (&optional arg)
  "Go to the arguments of the current function.

If ARG is non-nil, anonymous functions are skipped."
  (interactive "P")
  (bel-goto-function-name arg)
  (bel--forward-word 1)
  (forward-char 1))

(defun bel--goto-return-values (&optional arg)
  "Go to the declaration of return values for the current function."
  (bel-goto-arguments arg)
  (backward-char)
  (forward-list)
  (forward-char))

(defun bel-goto-return-values (&optional arg)
  "Go to the return value declaration of the current function.

If there are multiple ones contained in a parenthesis, enter the parenthesis.
If there is none, make space for one to be added.

If ARG is non-nil, anonymous functions are skipped."
  (interactive "P")
  (bel--goto-return-values arg)

  ;; Opening parenthesis, enter it
  (when (looking-at "(")
    (forward-char 1))

  ;; No return arguments, add space for adding
  (when (looking-at "{")
    (insert " ")
    (backward-char 1)))

(defun bel-goto-method-receiver (&optional arg)
  "Go to the receiver of the current method.

If there is none, add parenthesis to add one.

Anonymous functions cannot have method receivers, so when this is called
interactively anonymous functions will be skipped.  If called programmatically,
an error is raised unless ARG is non-nil."
  (interactive "P")

  (when (and (not (called-interactively-p 'interactive))
             (not arg)
             (bel--in-anonymous-funcion-p))
    (error "Anonymous functions cannot have method receivers"))

  (bel-goto-function t)  ; Always skip anonymous functions
  (forward-char 5)
  (when (not (looking-at "("))
    (save-excursion
      (insert "() ")))
  (forward-char 1))

(defun bel-goto-docstring (&optional arg)
  "Go to the top of the docstring of the current function.

If there is none, add one beginning with the name of the current function.

Anonymous functions do not have docstrings, so when this is called
interactively anonymous functions will be skipped.  If called programmatically,
an error is raised unless ARG is non-nil."
  (interactive "P")

  (when (and (not (called-interactively-p 'interactive))
             (not arg)
             (bel--in-anonymous-funcion-p))
    (error "Anonymous functions do not have docstrings"))

  (bel-goto-function t)
  (forward-line -1)
  (beginning-of-line)

  (while (looking-at "^//")
    (forward-line -1))
  (forward-line 1)
  (beginning-of-line)

  (cond
   ;; If we are looking at an empty comment, add a single space in front of it.
   ((looking-at "^//$")
    (forward-char 2)
    (insert (format " %s " (bel--function-name t))))
   ;; If we are not looking at the function signature, we are looking at a docstring.
   ;; Move to the beginning of the first word of it.
   ((not (looking-at "^func"))
    (forward-char 3))
   ;; If we are still at the function signature, we should add a new docstring.
   (t
    (forward-line -1)
    (newline)
    (insert "// ")
    (insert (bel--function-name t)))))

(defun bel--function-name (&optional arg)
  "Return the name of the surrounding function.

If ARG is non-nil, anonymous functions will be ignored and the
name returned will be that of the top-level function.  If ARG is
nil and the surrounding function is anonymous, nil will be
returned."
  (when (or (not (bel--in-anonymous-funcion-p))
            arg)
    (save-excursion
      (bel-goto-function-name t)
      (symbol-name (symbol-at-point)))))

(defun bel--in-anonymous-funcion-p ()
  "Return t if point is inside an anonymous function, nil otherwise."
  (save-excursion
    (bel-goto-function)
    (looking-at "\\<func(")))

(defun bel-guess-gopath (&optional buffer)
  "Determine a suitable GOPATH for BUFFER, or the current buffer if BUFFER is nil.

This function supports gb-based projects as well as Godep, in
addition to ordinary uses of GOPATH."
  (with-current-buffer (or buffer (current-buffer))
    (let ((gopath (cl-some (lambda (el) (funcall el))
                           bel-guess-gopath-functions)))
      (if gopath
          (mapconcat
           (lambda (el) (file-truename el))
           gopath
           path-separator)))))

(defun bel-plain-gopath ()
  "Detect a normal GOPATH, by looking for the first `src'
directory up the directory tree."
  (let ((d (locate-dominating-file buffer-file-name "src")))
    (if d
        (list d))))

(defun bel-godep-gopath ()
  "Detect a Godeps workspace by looking for Godeps/_workspace up
the directory tree. The result is combined with that of
`bel-plain-gopath'."
  (let* ((d (locate-dominating-file buffer-file-name "Godeps"))
         (workspace (concat d
                            (file-name-as-directory "Godeps")
                            (file-name-as-directory "_workspace"))))
    (if (and d
             (file-exists-p workspace))
        (list workspace
              (locate-dominating-file buffer-file-name "src")))))

(defun bel-gb-gopath ()
  "Detect a gb project."
  (or (bel--gb-vendor-gopath)
      (bel--gb-vendor-gopath-reverse)))

(defun bel--gb-vendor-gopath ()
  (let* ((d (locate-dominating-file buffer-file-name "src"))
         (vendor (concat d (file-name-as-directory "vendor"))))
    (if (and d
             (file-exists-p vendor))
        (list d vendor))))

(defun bel--gb-vendor-gopath-reverse ()
  (let* ((d (locate-dominating-file buffer-file-name "vendor"))
         (src (concat d (file-name-as-directory "src"))))
    (if (and d
             (file-exists-p src))
        (list d (concat d
                        (file-name-as-directory "vendor"))))))

(defun bel-wbel-gopath ()
  "Detect a wgo project."
  (or (bel--wbel-gocfg "src")
      (bel--wbel-gocfg "vendor")))

(defun bel--wbel-gocfg (needle)
  (let* ((d (locate-dominating-file buffer-file-name needle))
         (gocfg (concat d (file-name-as-directory ".gocfg"))))
    (if (and d
             (file-exists-p gocfg))
        (with-temp-buffer
          (insert-file-contents (concat gocfg "gopaths"))
          (append
           (mapcar (lambda (el) (concat d (file-name-as-directory el))) (split-string (buffer-string) "\n" t))
           (list (bel-original-gopath)))))))

(defun bel-set-project (&optional buffer)
  "Set GOPATH based on `bel-guess-gopath' for BUFFER, or the current buffer if BUFFER is nil.

If bel-guess-gopath returns nil, that is if it couldn't determine
a valid value for GOPATH, GOPATH will be set to the initial value
of when Emacs was started.

This function can for example be used as a
projectile-switch-project-hook, or simply be called manually when
switching projects."
  (interactive)
  (let ((gopath (or (bel-guess-gopath buffer)
                    (bel-original-gopath))))
    (setenv "GOPATH" gopath)
    (message "Set GOPATH to %s" gopath)))

(defun bel-reset-gopath ()
  "Reset GOPATH to the value it had when Emacs started."
  (interactive)
  (let ((gopath (bel-original-gopath)))
    (setenv "GOPATH" gopath)
    (message "Set GOPATH to %s" gopath)))

(defun bel-original-gopath ()
  "Return the original value of GOPATH from when Emacs was started."
  (let ((process-environment initial-environment)) (getenv "GOPATH")))

(defun bel--insert-modified-files ()
  "Insert the contents of each modified Go buffer into the
current buffer in the format specified by guru's -modified flag."
  (mapc #'(lambda (b)
            (and (buffer-modified-p b)
                 (buffer-file-name b)
                 (string= (file-name-extension (buffer-file-name b)) "bel")
                 (bel--insert-modified-file (buffer-file-name b) b)))
        (buffer-list)))

(defun bel--insert-modified-file (name buffer)
  (insert (format "%s\n%d\n" name (bel--buffer-size-bytes buffer)))
  (insert-buffer-substring buffer))

(defun bel--buffer-size-bytes (&optional buffer)
  (message "buffer; %s" buffer)
  "Return the number of bytes in the current buffer.
If BUFFER, return the number of characters in that buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (1- (position-bytes (point-max)))))

(provide 'bel-mode)

;;; bel-mode.el ends here
