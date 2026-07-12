;;; spthy-ts-mode.el --- tree-sitter support for the spthy language of the Tamarin prover  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ferdinand Nussbaum

;; Author: Ferdinand Nussbaum <ferdinand.nussbaum@inf.ethz.ch>
;; Version: 0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tamarin spthy languages tree-sitter
;; URL: https://www.github.com/fnussbaum/spthy-ts-mode

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'cl-lib)
(eval-when-compile (require 'rx))

;; TODO rename to spthy-ts-indent-offset
(defcustom spthy-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `spthy-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'spthy)

;; Adapted from `c-ts-mode-toggle-comment-style'.
(defun spthy-ts-mode-toggle-comment-style (&optional arg)
  "Toggle the comment style between block and line comments.
Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or omitted."
  (interactive "P")
  (let ((prevstate-line (string= comment-start "// ")))
    (when (or (not arg)
              (zerop (setq arg (prefix-numeric-value arg)))
              (xor (> 0 arg) prevstate-line))
      (pcase-let ((`(,starter . ,ender)
                   (if prevstate-line
                       (cons "/* " " */")
                     (cons "// " ""))))
        (setq-local comment-start starter
                    comment-end ender)))))

;;; Syntax table

(defvar spthy-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_   "_"      table)
    (modify-syntax-entry ?%   "_"      table)
    (modify-syntax-entry ?$   "_"      table)
    (modify-syntax-entry ?~   "_"      table)
    ;; The exclamation mark for persistent facts
    ;; can be considered part of their name.
    (modify-syntax-entry ?!   "_"      table)
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?⊕   "."      table)
    (modify-syntax-entry ?∃   "."      table)
    (modify-syntax-entry ?∀   "."      table)
    (modify-syntax-entry ?∨   "."      table)
    (modify-syntax-entry ?∧   "."      table)
    (modify-syntax-entry ?¬   "."      table)
    (modify-syntax-entry ?⇒   "."      table)
    (modify-syntax-entry ?⇔   "."      table)
    (modify-syntax-entry ?\'  "\""     table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)
    table))

;; Adapted from `c-ts-mode--syntax-propertize'.
(defun spthy-ts-mode--syntax-propertize (beg end)
  "Apply syntax text property to template delimiters between BEG and END.

< and > are usually punctuation, e.g., in ]->, or as comparison operators;
but when used for tuples, they should be considered pairs.
A period . may be part of a variable identifier.
The replication operator ! needs to be considered punctuation.

This function checks for <, >, . and ! characters in the changed RANGES and
applies the appropriate text property to alter their syntax class."
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">" "." "!")) end t)
    (let* ((node (treesit-node-at (match-beginning 0)))
          (node-type (treesit-node-type node))
          (parent-type (treesit-node-type (treesit-node-parent node))))
      (cond
       ;; FIXME Try making electric pairs work (handle error nodes)
       ((and (member node-type '("<" ">"))
             (equal parent-type "tuple_term"))
        (put-text-property (match-beginning 0)
                           (match-end 0)
                           'syntax-table
                           (pcase (char-before)
                             (?< '(4 . ?>))
                             (?> '(5 . ?<)))))
       ((and (equal node-type ".")
             (member parent-type
                     '("pub_var" "fresh_var" "nat_var"
                       "msg_var_or_nullary_fun" "temporal_var"
                       "temporal_var_optional_prefix")))
        (put-text-property (match-beginning 0)
                           (match-end 0)
                           'syntax-table
                           '(3 . ?.)))
       ((and (equal node-type "!")
             (equal parent-type "replication"))
        (put-text-property (match-beginning 0)
                           (match-end 0)
                           'syntax-table
                           '(1 . ?!)))))))

;;; Font-lock

(defvar spthy-ts-mode--tokens
  '((general "theory" "begin" "end" "builtins" "functions" "export"
             "options" "equations" "predicates" "macros" "heuristic"
             "tactic" "rule" "variants" "axiom" "restriction" "process"
             "lemma" "diffLemma" "all-traces" "exists-trace" "All" "Ex"
             (let ("let")) (rule_let_block "let") (rule_let_block "in")
             "fresh" "not" "test" "accounts" "account" "for"
             "equivLemma" "diffEquivLemma")
    (proof "next" "case" "by" "ATTACK" ((solved)) ((mirrored)) "qed"
           "contradiction" "backward-search" "simplify"
           "induction" "rule-equivalence")
    (tactic "presort" "prio" "deprio" "smallest" "id")
    (tactic-function "regex" "isFactName" "isInFactTerms" "dhreNoise"
                     "defaultNoise" "reasonableNoncesNoise" "nonAbsurdConstraint")
    (preprocessor "#ifdef" "#else" "#endif" "#define" "#include")
    (quiet "modulo" "$" "~")
    (processes "out" (process_let "let") (process_let "in")
               (input "in") (read_state "in")
               "new" "lookup" "lock" "unlock" "delete" "insert"
               "event" "as" "if" "then" "else")
    (brackets "(" ")" "<" ">")
    (rule-delimiters "--[" "]->" "[" "]" "-->")
    ;; The "not" operator is highlighted like a keyword.
    (operators "&" "∧" "|" "∨" "==>" "⇒" "<=>" "⇔" "¬"
               "||" (replication "!") (non_deterministic_choice "+"))
    (delimiters "," ":" "@"))
   "Tamarin spthy tokens for tree-sitter font-locking.")

(defvar spthy-ts-mode--builtin-functions
  '((hashing "h")
    (asymmetric-encryption "adec" "aenc" "pk")
    (signing "sign" "verify" "pk" "true")
    (revealing-signing "revealSign" "revealVerify" "getMessage" "pk" "true")
    (symmetric-encryption "senc" "sdec")
    ;; also "^" and "*" operators
    (diffie-hellman "inv" "1" "DH_neutral")
    (bilinear-pairing "inv" "1" "DH_neutral" "pmult" "em")
    (xor "zero")))

;; Used for semantic highlighting. A current limitation is that
;; this only queries the current buffer, and does not collect
;; information from included files.
(defmacro spthy-ts-mode--throttled-query-function (query collect)
  `(let ((last-time 0)
         (last-value nil)
         (query ,query))
     (lambda ()
       (let ((current-time (time-convert (current-time) 'integer)))
         (if (> current-time
                (+ last-time 2))
             (setq
              last-time current-time
              last-value
              (cl-loop
               for (_ . node) in
               (treesit-query-capture 'spthy query)
               collect ,collect))
           last-value)))))

(defalias 'spthy-ts-mode--imported-theories
  (spthy-ts-mode--throttled-query-function
   (treesit-query-compile 'spthy '((built_in) @builtin))
   (treesit-node-type (treesit-node-child node 0))))

(defun spthy-ts-mode--add-face-builtin-function
    (node _override start end &rest _)
  (when (cl-intersection
         (cl-loop for (theory . idents) in spthy-ts-mode--builtin-functions
                  when (member (treesit-node-text node) idents)
                  collect (symbol-name theory))
         (spthy-ts-mode--imported-theories)
         :test #'equal)
    (add-face-text-property
     (max (treesit-node-start node) start)
     (min (treesit-node-end node) end)
     'font-lock-builtin-face)))

(defalias 'spthy-ts-mode--predefined-processes
  (spthy-ts-mode--throttled-query-function
   (treesit-query-compile 'spthy '((let (mset_term) @process)))
   (treesit-node-text
    (treesit-node-at (treesit-node-start node)))))

(defun spthy-ts-mode--add-face-process-identifier
    (node _override start end &rest _)
  (let ((ident (treesit-node-at (treesit-node-start node))))
    (add-face-text-property
     (max (treesit-node-start ident) start)
     (min (treesit-node-end ident) end)
     (when (or (treesit-node-match-p (treesit-node-parent node) "^let$")
               (cl-member-if
                (lambda (id)
                  (equal id (treesit-node-text ident)))
                (spthy-ts-mode--predefined-processes)))
       'font-lock-variable-name-face))))

(defvar spthy-ts-mode--builtin-facts
  '("In" "Out" "Fr"))

(defvar spthy-ts-mode--builtin-action-constraints
  '("K" "KU" "KD"))

(defun spthy-ts-mode--fact-font-lock-enabled ()
  (cl-loop for setting in treesit-font-lock-settings
           thereis (and (eq (treesit-font-lock-setting-feature setting) 'fact)
                        (treesit-font-lock-setting-enable setting))))

;; The distinction between variable-use, variable-name faces might not
;; make too much sense.
(cl-loop
 for (name builtin-face other-face builtin-list)
 in '((spthy-ts-mode--add-face-action-constraint
       font-lock-builtin-face font-lock-property-use-face
       spthy-ts-mode--builtin-action-constraints)
      (spthy-ts-mode--add-face-premise-fact
       font-lock-builtin-face font-lock-variable-use-face
       spthy-ts-mode--builtin-facts)
      (spthy-ts-mode--add-face-conclusion-fact
       font-lock-builtin-face font-lock-variable-name-face
       spthy-ts-mode--builtin-facts))
 do
 (eval `(defun ,name (node _override start end &rest _)
          (add-face-text-property
           (max (treesit-node-start node) start)
           (min (treesit-node-end node) end)
           (cond
            ((member (treesit-node-text node) ,builtin-list)
             ',builtin-face)
            ((spthy-ts-mode--fact-font-lock-enabled)
             ',other-face))))))

(defun spthy-ts-mode--tokens-add-face (type face)
  (apply
   #'vector
   (mapcar
    (lambda (token) (append (ensure-list token) (list face)))
    (alist-get type spthy-ts-mode--tokens))))

(defvar spthy-ts-mode--font-lock-rules
  `( :language spthy
     :feature comment
     (([(multi_comment) (single_comment)] @font-lock-comment-face)
      ( formal_comment comment_identifier: (ident) @font-lock-constant-face
        (:match ,(regexp-opt '("section" "subsection" "text") 'symbols)
                @font-lock-constant-face)))

     :language spthy
     :feature constant
     (((pub_name) @font-lock-string-face))

     :language spthy
     :feature operator
     (,(spthy-ts-mode--tokens-add-face
        'operators '@font-lock-operator-face)
      ,(spthy-ts-mode--tokens-add-face
        'rule-delimiters '@font-lock-delimiter-face))

     ;; Inspired by `spthy-mode'.
     :language spthy
     :feature quiet
     (,(spthy-ts-mode--tokens-add-face 'quiet '@font-lock-comment-face)
      (pub_var ["pub" ":"] @font-lock-comment-face)
      (fresh_var ["fresh" ":"] @font-lock-comment-face)
      (msg_var_or_nullary_fun ["msg" ":"] @font-lock-comment-face)
      (temporal_var ["node" ":"] @font-lock-comment-face)
      (nat_var ["nat" ":"] @font-lock-comment-face)
      (any_var ["ANY" ":"] @font-lock-comment-face))

     :language spthy
     :feature keyword
     (,(spthy-ts-mode--tokens-add-face
        'general '@font-lock-keyword-face)
      ,(spthy-ts-mode--tokens-add-face
        'preprocessor '@font-lock-preprocessor-face)
      ((atom) @font-lock-keyword-face))

     :language spthy
     :feature process
     (,(spthy-ts-mode--tokens-add-face
        'processes '@font-lock-keyword-face)
      (predefined_process (mset_term) @spthy-ts-mode--add-face-process-identifier))

     :language spthy
     :feature definition
     ((simple_rule rule_identifier: (ident) @font-lock-function-name-face)
      (restriction restriction_identifier: (ident) @font-lock-function-name-face)
      (lemma lemma_identifier: (ident) @font-lock-function-name-face)
      (diff_lemma lemma_identifier: (ident) @font-lock-function-name-face)
      (case_test test_identifier: (ident) @font-lock-variable-name-face)
      (accountability_lemma lemma_identifier: (ident) @font-lock-function-name-face)
      (tactic (ident) @font-lock-function-name-face)
      (let let_identifier: (mset_term) @spthy-ts-mode--add-face-process-identifier))

     :language spthy
     :feature fact
     ((premise ( linear_fact fact_identifier: (ident)
                 @font-lock-variable-use-face))
      (premise ( persistent_fact fact_identifier: (ident)
                 @font-lock-variable-use-face))
      (conclusion ( linear_fact fact_identifier: (ident)
                    @font-lock-variable-name-face))
      (conclusion ( persistent_fact fact_identifier: (ident)
                    @font-lock-variable-name-face)))

     :language spthy
     :feature action-fact
     ((action_fact ( linear_fact fact_identifier: (ident)
                    @font-lock-property-name-face))
      (action_fact ( persistent_fact fact_identifier: (ident)
                     @font-lock-property-name-face))
      (action_constraint ( linear_fact fact_identifier: (ident)
                           @font-lock-property-use-face))
      (action_constraint ( persistent_fact fact_identifier: (ident)
                           @font-lock-property-use-face)))

     :language spthy
     :feature builtin
     :override t
     (((nary_app function_identifier: (ident)
                 @spthy-ts-mode--add-face-builtin-function))
      ((built_in) @font-lock-builtin-face)
      (action_constraint ( linear_fact fact_identifier: (ident)
                           @spthy-ts-mode--add-face-action-constraint))
      (action_constraint ( persistent_fact fact_identifier: (ident)
                           @spthy-ts-mode--add-face-action-constraint))
      (premise ( linear_fact fact_identifier: (ident)
                 @spthy-ts-mode--add-face-premise-fact))
      (premise ( persistent_fact fact_identifier: (ident)
                 @spthy-ts-mode--add-face-premise-fact))
      (conclusion ( linear_fact fact_identifier: (ident)
                    @spthy-ts-mode--add-face-conclusion-fact))
      (conclusion ( persistent_fact fact_identifier: (ident)
                    @spthy-ts-mode--add-face-conclusion-fact)))

     :language spthy
     :feature proof
     (,(spthy-ts-mode--tokens-add-face 'proof '@font-lock-keyword-face)
      (["step" "solve"] @font-lock-function-call-face)
      (["sorry"] @font-lock-warning-face))

     :language spthy
     :feature tactic
     (,(spthy-ts-mode--tokens-add-face
        'tactic '@font-lock-preprocessor-face)
      ,(spthy-ts-mode--tokens-add-face
        'tactic-function '@font-lock-preprocessor-face))))

;;; Indentation

(defun spthy-ts-mode--largest-node-at (pos)
  (treesit-parent-while
   (treesit-node-at pos)
   (lambda (node)
     (and (eq pos (treesit-node-start node))
          (not (treesit-node-eq node (treesit-buffer-root-node)))))))

(defun spthy-ts-mode--prev-line-node (pos)
  (save-excursion
    (goto-char pos)
    (if (eq (forward-line -1) 0)
        (progn (back-to-indentation) (spthy-ts-mode--largest-node-at (point))))))

(defun spthy-ts-mode--prev-line-error (_node _parent bol &rest _)
  (let ((prev-line-node (spthy-ts-mode--prev-line-node bol)))
    (or (treesit-node-match-p prev-line-node "^ERROR$")
        (treesit-node-match-p (treesit-node-parent prev-line-node) "^ERROR$"))))

(defun spthy-ts-mode--closing-rule-bracket-p (node parent bol &rest _)
  (let ((type (treesit-node-type node)))
    (or (member type '("]" "]->"))
        (and (equal type "ERROR")
             (treesit-node-match-p parent "^action_fact$")
             (save-excursion
               (goto-char bol)
               (or (looking-at-p
                    (concat "[[:blank:]]*" "]->"))
                   (looking-at-p
                    (concat "[[:blank:]]*" "]"))))))))

(defun spthy-ts-mode--prev-matching-bracket-node (node)
  (let* ((type (treesit-node-type node))
         (matching-bracket
          (save-excursion
            (goto-char (treesit-node-start node))
            (pcase type
              (">"   "<")
              (")"   "(")
              ((or
                "]->"
                (guard (looking-at-p
                        (concat "[[:blank:]]*" "]->"))))
               "--[")
              ((or
                "]"
                (and "ERROR"
                     (guard (looking-at-p
                             (concat "[[:blank:]]*" "]")))))
               "[")))))
    (while (and node (not (equal (treesit-node-type node) matching-bracket)))
      (setq node (treesit-node-prev-sibling node)))
    node))

(defun spthy-ts-mode--prev-matching-bracket-start (node &rest _)
  ;; For --[, we need the location of the bracket [.
  (- (treesit-node-end
      (spthy-ts-mode--prev-matching-bracket-node node))
     1))

(defun spthy-ts-mode--logical-operator-indent-rule (node parent &rest _)
  (when (or (treesit-node-match-p
             node
             (spthy-ts-mode--regexp-opt-line
              '("&" "∧" "|" "∨" "==>" "⇒" "<=>" "⇔" "not" "¬")))
            (treesit-node-match-p
             parent
             (spthy-ts-mode--regexp-opt-line
              '("conjunction" "disjunction" "imp" "iff" "negation"))))
    (cl-flet* ((above-parent-line-p (nod)
                 (< (treesit-node-start nod)
                    (save-excursion
                      (goto-char (treesit-node-start parent))
                      (pos-bol))))
               (parent-quantifier-prev-line (nod)
                 (let ((nod-parent (treesit-node-parent nod)))
                   (or (equal (treesit-node-type nod-parent) "quantified_formula")
                       (above-parent-line-p nod-parent)))))
      (let* ((maybe-quantifier-child
              (treesit-parent-until parent #'parent-quantifier-prev-line
                                    'include-node))
             (maybe-quantifier
              (treesit-node-parent maybe-quantifier-child)))
        (if (above-parent-line-p maybe-quantifier)
            `(,(treesit-node-start parent) . 0)
          `(,(treesit-node-start maybe-quantifier) .
            ,(+ spthy-ts-mode-indent-offset
                (- (treesit-node-start parent)
                   (treesit-node-start
                    maybe-quantifier-child)))))))))

(defun spthy-ts-mode--incomplete-let-indent-rule (_node _parent bol &rest _)
  (when-let* ((node (spthy-ts-mode--prev-non-comment-node bol))
              (let-pos
               (when (equal (treesit-node-type node) "let")
                 (treesit-node-start node))))
    `(,let-pos . ,spthy-ts-mode-indent-offset)))

(defun spthy-ts-mode--indent-try-insertions (candidates bol)
  (let ((prefix (buffer-substring-no-properties (point-min) bol))
        (indent-rules treesit-simple-indent-rules)
        (result nil))
    (with-temp-buffer
      (delay-mode-hooks (spthy-ts-mode))
      (setq-local treesit-simple-indent-rules indent-rules)
      (insert prefix)
      (let ((temp-bol (point)))
        (catch 'result
          (dolist (str candidates)
            (insert str)
            (let ((nod (treesit-node-at temp-bol)))
              (when (and (equal (treesit-node-type nod) str)
                         (not (equal (treesit-node-type (treesit-node-parent nod))
                                     "ERROR")))
                (throw 'result
                       (setq result
                             (treesit-simple-indent
                              nod (treesit-node-parent nod) temp-bol)))))
            (delete-region temp-bol (point))))))))

(defun spthy-ts-mode--missing-closing-bracket-indent-rule
    (node parent bol &rest _)
  (when (and (null node)
             (spthy-ts-mode--prev-line-error node parent bol))
    (spthy-ts-mode--indent-try-insertions '("]" "]->" ")" ">") bol)))

(defvar spthy-ts-mode--missing-query
  (treesit-query-compile 'spthy '((MISSING) @missing)))

(defun spthy-ts-mode--parser-missing-node-indent-rule
    (node _parent bol &rest _)
  (when-let* ((_ (null node))
              (missing-node
               (cdar
                (let ((pos (treesit-node-end
                            (spthy-ts-mode--prev-non-comment-node bol))))
                  (cl-member-if
                   (pcase-lambda (`(,_ . ,nod))
                     (equal (treesit-node-start nod) pos))
                   (treesit-query-capture
                    'spthy spthy-ts-mode--missing-query))))))
    (spthy-ts-mode--indent-try-insertions
     (list (treesit-node-type missing-node)) bol)))

(defun spthy-ts-mode--within-proof-p (node &rest _)
  (cl-flet ((proof-node-p (nod)
              (or (member (treesit-node-type nod)
                          '("cases" "case"))
                  (member (treesit-node-field-name nod)
                          '("proof_skeleton")))))
    (treesit-parent-until
     node
     #'proof-node-p 'include-node)))

(defun spthy-ts-mode--non-comment-node-p (node)
  (not (member (treesit-node-type node) '("single_comment" "multi_comment"))))

(defun spthy-ts-mode--prev-non-comment-node (bol)
  (save-excursion
    (goto-char bol)
    (treesit-search-forward-goto
     (treesit-node-at (point))
     #'spthy-ts-mode--non-comment-node-p 'start 'backward 'all)
    (treesit-node-at (point))))

(defun spthy-ts-mode--prev-node-is (node-t &optional parent-t prev-line)
  (lambda (_node _parent bol &rest _)
    (let ((node (spthy-ts-mode--prev-non-comment-node bol)))
      (and
       (not (and prev-line
                 ;; The parameter prev-line enforces that there
                 ;; should be no blank line between the last
                 ;; non-comment node and the line to indent.
                 (save-excursion
                   (goto-char bol)
                   (cl-loop while
                            (and (= 0 (forward-line -1))
                                 (>= (point) (treesit-node-start node)))
                            thereis (looking-at-p "[[:blank:]]*$")))))
       (or (null node-t)
           (string-match-p
            node-t (or (treesit-node-type node) "")))
       (or (null parent-t)
           (string-match-p
            parent-t
            (treesit-node-type
             (treesit-node-parent node))))))))

(defun spthy-ts-mode--formula-quote-before-p
    (_node parent &rest _)
  (when-let*
      ((quote-child
        (and
         (member
          (treesit-node-type parent)
          '("lemma" "restriction" "case_test" "accountability_lemma"))
         (car
          (treesit-filter-child
           parent
           (lambda (nod) (equal (treesit-node-type nod) "\"")))))))
    (< (treesit-node-start quote-child) (point))))

(defun spthy-ts-mode--end-of-prev-node
    (_node _parent bol)
  (treesit-node-end (spthy-ts-mode--prev-non-comment-node bol)))

;; Adapted from `standalone-parent' in `treesit-simple-indent-presets'.
(defun spthy-ts-mode--nested-or-standalone-parent (_node parent &rest _)
  (save-excursion
    (catch 'term
      (while parent
        (goto-char (treesit-node-start parent))
        (when (or (treesit-node-match-p (treesit-node-parent parent)
                                        "^nested_process$")
                  (looking-back (rx bol (* whitespace))
                                (line-beginning-position)))
          (throw 'term (point)))
        (setq parent (treesit-node-parent parent))))))

(defun spthy-ts-mode--prev-sibling-line-first-sibling (node parent bol &rest _)
  (when-let* ((prev-sibling
               ;; From `prev-sibling' in `treesit-simple-indent-presets'.
               (or (treesit-node-prev-sibling node t)
                   (treesit-node-prev-sibling
                    (treesit-node-first-child-for-pos
                     parent bol)
                    t)
                   (treesit-node-child parent -1 t)))
              (prev-sibling-bol
               (save-excursion
                 (goto-char (treesit-node-start prev-sibling))
                 (pos-bol)))
              (nod prev-sibling))
    (while
        (and-let* ((prev-sibling (treesit-node-prev-sibling nod t))
                   ((>= (treesit-node-start prev-sibling)
                        prev-sibling-bol)))
          (setq nod prev-sibling)))
    (treesit-node-start nod)))

(defvar spthy-ts-mode--process-nodes
  '("set_lock" "remove_lock" "input" "read_state" "delete_state"
    "set_state" "output" "event" "process_let" "binding"
    "conditional" "predefined_process"))

(defun spthy-ts-mode--regexp-opt-line (strings)
  (concat "^" (regexp-opt strings) "$"))

;; TODO allow trailing commas in fact lists in grammar, see if can eliminate some edge cases
;; TODO indent "predicates: ..."
;; TODO embedded restrictions
(defvar spthy-ts-mode--indent-settings
  `((spthy
     ;; Don't interfere with proof formatting.
     (spthy-ts-mode--within-proof-p
      no-indent)
     ((and no-node (spthy-ts-mode--prev-node-is ":" nil t))
      parent ,spthy-ts-mode-indent-offset)
     ;; Handle incomplete rules.
     ((spthy-ts-mode--prev-node-is ,(spthy-ts-mode--regexp-opt-line '("[" "--[")) nil t)
      spthy-ts-mode--end-of-prev-node 1)
     ;; Handle the case where the parser itself does not
     ;; recover with a MISSING node.
     spthy-ts-mode--missing-closing-bracket-indent-rule
     ;; Handle the case where the parser does recover.
     spthy-ts-mode--parser-missing-node-indent-rule
     ((parent-is "^inline_msr_process$")
      parent 0)
     ((node-is "^macro$")
      parent ,spthy-ts-mode-indent-offset)
     ((or (parent-is "^theory$")
          (parent-is "^tactic$"))
      column-0 0)
     ((spthy-ts-mode--prev-node-is "^,$" nil t)
      spthy-ts-mode--prev-sibling-line-first-sibling 0)
     ((and (parent-is "^quantified_formula$")
           (field-is "^variable$"))
      (nth-sibling 1) 0)
     ((and (parent-is "^quantified_formula$")
           (node-is "^\\.$"))
      parent 0)
     ((or (parent-is "^quantified_formula$")
          (node-is "^arguments$"))
      parent ,spthy-ts-mode-indent-offset)
     ((or (n-p-gp "\""
                  ,(spthy-ts-mode--regexp-opt-line
                    '("lemma" "restriction" "case_test"
                      "accountability_lemma"))
                  nil)
          (node-is "^trace_quantifier$"))
      column-0 ,spthy-ts-mode-indent-offset)
     spthy-ts-mode--logical-operator-indent-rule
     ((and no-node spthy-ts-mode--formula-quote-before-p)
      prev-line 0) ; for writing formulas
     ((n-p-gp ")" "^nested_process$" nil)
      parent 0)
     ((parent-is "^nested_process$")
      parent 1)
     ((and (parent-is "^conditional$")
           (or (node-is "^nested_process$")
               (node-is ,(spthy-ts-mode--regexp-opt-line
                          spthy-ts-mode--process-nodes))))
      parent ,spthy-ts-mode-indent-offset)
     ((parent-is ,(spthy-ts-mode--regexp-opt-line
                   spthy-ts-mode--process-nodes))
      spthy-ts-mode--nested-or-standalone-parent 0)
     ((n-p-gp "^]$"
              ,(spthy-ts-mode--regexp-opt-line
                '("premise" "conclusion"))
              nil)
      parent 0)
     ((n-p-gp "^]->$" "^action_fact$" nil)
      parent 2)
     ((n-p-gp "^in$" "^rule_let_block$" nil)
      parent 0)
     ((node-is "^-->$")
      column-0 ,(max 0 (- spthy-ts-mode-indent-offset 1)))
     ((or (node-is "^action_fact$")
          ;; Handle incomplete rules.
          (spthy-ts-mode--prev-node-is "^\\]$" "^premise$"))
      column-0 ,(max 0 (- spthy-ts-mode-indent-offset 1)))
     ((or (node-is "^premise$")
          (node-is "^conclusion$")
          (spthy-ts-mode--prev-node-is "^\\]->$" "^action_fact$")
          (spthy-ts-mode--prev-node-is "^-->$"))
      column-0 ,spthy-ts-mode-indent-offset)
     spthy-ts-mode--incomplete-let-indent-rule
     ;; Handle first fact within rule premises and conclusions.
     ((match nil ,(spthy-ts-mode--regexp-opt-line
                   '("premise" "conclusion"))
             nil 1 1)
      parent 2)
     ;; Handle first action fact.
     ((match nil "^action_fact$"
             nil 1 1)
      parent 4)
     ((or (node-is ")") (node-is "]") (node-is ">")
          ;; Handle the case of comma and empty line:
          ;; --[ A(x),
          ;;
          ;;  |]->
          spthy-ts-mode--closing-rule-bracket-p)
      spthy-ts-mode--prev-matching-bracket-start 0)
     ((or (n-p-gp nil nil "^theory$")
          (parent-is "^simple_rule$")
          (n-p-gp nil nil "^tactic$")
          (parent-is "^rule_let_block$"))
      first-sibling ,spthy-ts-mode-indent-offset)
     ((n-p-gp "^,$" "^action_fact$" nil)
      parent 2)
     ((and no-node (or (parent-is "^premise$")
                       (parent-is "^conclusion$")))
      parent 0)
     ((and no-node (parent-is "^action_fact$"))
      parent 2)
     (no-node prev-line 0)
     (catch-all parent 0))))

;;; Things

(defun spthy-ts-mode--node-defun-p (node)
  (and (equal (treesit-node-type (treesit-node-parent node)) "theory")
       (not (member (treesit-node-type node)
                    '( nil "single_comment" "multi_comment"
                       "theory" "begin" "end" "ident")))))

(defvar spthy-ts-mode--treesit-things
  '((spthy
     (defun spthy-ts-mode--node-defun-p))))

;;; Imenu

(defun spthy-ts-mode--defun-name (node)
  (pcase (treesit-node-type node)
    ((or "lemma" "diff_lemma" "restriction" "case_test" "accountability_lemma")
     (concat
      (treesit-node-text
       (or (treesit-node-child-by-field-name node "lemma_identifier")
           (treesit-node-child-by-field-name node "restriction_identifier")
           (treesit-node-child-by-field-name node "test_identifier")))
      (mapconcat
       #'treesit-node-text
       (treesit-filter-child
        node
        (lambda (nod)
          (member (treesit-node-type nod)
                  '("diff_lemma_attrs" "restriction_attr")))))))
    ("rule"
     (treesit-node-text (treesit-node-child-by-field-name
                         (treesit-node-child node 0 t) "rule_identifier")))
    ("process" "process:")
    ("let"
     (treesit-node-text (treesit-node-child-by-field-name
                         node "let_identifier")))))

(defun spthy-ts-mode--parent-is-theory-p (node)
  (treesit-node-match-p (treesit-node-parent node) "^theory$"))

(defvar spthy-ts-mode--imenu-settings
  `(( "Process" ,(spthy-ts-mode--regexp-opt-line
                  '("process" "let"))
      spthy-ts-mode--parent-is-theory-p nil)
    ( "Rule" "^rule$"
      spthy-ts-mode--parent-is-theory-p nil)
    ( "Restriction" "^restriction$"
      spthy-ts-mode--parent-is-theory-p nil)
    ( "Lemma" "^lemma$"
      spthy-ts-mode--parent-is-theory-p nil)
    ( "Case Test" "^case_test$"
      spthy-ts-mode--parent-is-theory-p nil)
    ( "Accountability Lemma" "^accountability_lemma$"
      spthy-ts-mode--parent-is-theory-p nil)
    ( "diffLemma" "^diff_lemma$"
      spthy-ts-mode--parent-is-theory-p nil)))

(with-eval-after-load 'consult-imenu
  (add-to-list 'consult-imenu-config
               '(spthy-ts-mode
                 :types
                 ((?p "Process" font-lock-variable-name-face)
                  (?r "Rule" font-lock-variable-name-face)
                  (?R "Restriction" font-lock-function-name-face)
                  (?l "Lemma" font-lock-function-name-face)
                  (?c "Case Test" font-lock-variable-name-face)
                  (?a "Accountability Lemma" font-lock-function-name-face)
                  (?d "diffLemma" font-lock-function-name-face)))))

;;; Mode

;;;###autoload
(define-derived-mode spthy-ts-mode prog-mode "spthy-ts"
  "Major mode for editing the spthy language of the Tamarin prover."
  :syntax-table spthy-ts-mode--syntax-table

  (when (treesit-ready-p 'spthy)
    (treesit-parser-create 'spthy)

    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       spthy-ts-mode--font-lock-rules))
    (setq-local syntax-propertize-function #'spthy-ts-mode--syntax-propertize)

    (setq-local treesit-simple-indent-rules spthy-ts-mode--indent-settings)

    (setq-local treesit-defun-name-function #'spthy-ts-mode--defun-name)
    (setq-local treesit-defun-tactic 'top-level)
    (setq-local treesit-simple-imenu-settings spthy-ts-mode--imenu-settings)
    (setq-local treesit-thing-settings spthy-ts-mode--treesit-things)

    (setq-local comment-start "// ")
    (setq-local comment-end "")

    (when (boundp 'electric-pair-pairs)
      (setq-local electric-pair-pairs
                  (cons
                   '(("--\\[" . "]->"))
                   electric-pair-pairs)))

    (setq-local treesit-font-lock-feature-list
                '(( comment)
                  ( constant quiet)
                  ( keyword tactic proof definition
                    action-fact builtin process operator)
                  ( fact delimiter)))

    (treesit-major-mode-setup)
    (treesit-inspect-mode)))

(defun spthy-ts-mode--sp-premise-or-conclusion-p ()
  (or
   (funcall (spthy-ts-mode--prev-node-is ":" "^simple_rule$")
            nil nil (- (point) 1))
   (and (funcall (spthy-ts-mode--prev-node-is ":" "^ERROR$")
                 nil nil (- (point) 1))
        (treesit-node-match-p
         (treesit-node-at
          (save-excursion
            (goto-char (treesit-node-start
                        (spthy-ts-mode--prev-non-comment-node (- (point) 1))))
            (pos-bol)))
         "^rule$"))
   (funcall (spthy-ts-mode--prev-node-is "]->" "^action_fact$")
            nil nil (- (point) 1))))

(defun spthy-ts-mode--sp-square-bracket-handler (_id _action _context)
  (when (spthy-ts-mode--sp-premise-or-conclusion-p)
    (insert " ")
    (save-excursion
      (insert " "))
    (indent-for-tab-command)))

(defun spthy-ts-mode--sp-action-fact-p (_id _action _context)
  (funcall (spthy-ts-mode--prev-node-is "]" "^premise$")
           nil nil (- (point) 3)))

(with-eval-after-load 'smartparens
  (sp-with-modes '(spthy-ts-mode)
    (sp-local-pair "--[" "]->"
                   :when '(spthy-ts-mode--sp-action-fact-p)
                   :post-handlers '(" || "))
    (sp-local-pair "[" "]"
                   :post-handlers
                   '(spthy-ts-mode--sp-square-bracket-handler))))

;;;###autoload
(with-eval-after-load 'treesit
  (add-to-list
   'treesit-language-source-alist
   '(spthy "https://github.com/fnussbaum/tamarin-prover"
           :source-dir "tree-sitter/tree-sitter-spthy/src"
           :commit "6e0cc1c7f82b277f5ccfc6b735cdf101d36f62e8")
   t)
  (when (treesit-ready-p 'spthy)
    (add-to-list 'auto-mode-alist '("\\.spthy\\'" . spthy-ts-mode))))

(provide 'spthy-ts-mode)

;;; spthy-ts-mode.el ends here
