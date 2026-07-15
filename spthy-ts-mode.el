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
(require 'c-ts-common) ; for comment indentation and filling
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

(defconst spthy-ts-mode--tokens
  '((general "theory" "begin" "end" "builtins" "functions" "export"
             "options" "equations" "predicates" "macros" "heuristic"
             "options" "tactic" "rule" "variants" "axiom" "restriction"
             "process" "lemma" "diffLemma" "all-traces" "exists-trace"
             "All" "Ex" (let ("let")) (rule_let_block "let")
             (rule_let_block "in") "fresh" "not" "test" "accounts"
             "account" "for" "equivLemma" "diffEquivLemma")
    (proof "next" "case" "by" "ATTACK" ((solved)) ((mirrored)) "qed"
           "contradiction" "backward-search" "simplify"
           "induction" "rule-equivalence")
    (tactic "presort" "prio" "deprio" "smallest" "id")
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

(defconst spthy-ts-mode--builtin-functions
  '((hashing "h")
    (asymmetric-encryption "adec" "aenc" "pk")
    (signing "sign" "verify" "pk" "true")
    (revealing-signing "revealSign" "revealVerify" "getMessage" "pk" "true")
    (symmetric-encryption "senc" "sdec")
    ;; also includes "^" and "*" operators
    (diffie-hellman "inv" "1" "DH_neutral")
    (bilinear-pairing "inv" "1" "DH_neutral" "pmult" "em")
    (xor "zero")))

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
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (when
        (and
         (<= start node-start node-end end)
         (let ((imported-theories (spthy-ts-mode--imported-theories)))
           (cl-loop for (theory . idents) in spthy-ts-mode--builtin-functions
                    thereis
                    (and (member (treesit-node-text node) idents)
                         (member (symbol-name theory) imported-theories)))))
      (add-face-text-property
       node-start node-end 'font-lock-builtin-face))))

(defalias 'spthy-ts-mode--predefined-processes
  (spthy-ts-mode--throttled-query-function
   (treesit-query-compile 'spthy '((let (mset_term) @process)))
   (treesit-node-text
    (treesit-node-at (treesit-node-start node)))))

(defun spthy-ts-mode--add-face-process-identifier
    (node _override start end &rest _)
  (let* ((ident (treesit-node-at (treesit-node-start node)))
         (ident-start (treesit-node-start ident))
         (ident-end (treesit-node-end ident)))
    (when
        (and (<= start ident-start ident-end end)
             (or (treesit-node-match-p (treesit-node-parent node) "^let$")
                 (cl-member-if
                  (lambda (id)
                    (equal id (treesit-node-text ident)))
                  (spthy-ts-mode--predefined-processes))))
      (add-face-text-property
       ident-start ident-end 'font-lock-variable-name-face))))

(defconst spthy-ts-mode--builtin-facts
  '("In" "Out" "Fr"))

(defconst spthy-ts-mode--builtin-action-constraints
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
          (let ((node-start (treesit-node-start node))
                (node-end (treesit-node-end node)))
            (when (<= start node-start node-end end)
              (add-face-text-property
               node-start node-end
               (cond
                ((member (treesit-node-text node) ,builtin-list)
                 ',builtin-face)
                ((spthy-ts-mode--fact-font-lock-enabled)
                 ',other-face))))))))

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
     ((pub_name) @font-lock-string-face)

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
      ((atom) @font-lock-keyword-face)
      ("configuration" @font-lock-constant-face)
      ((option) @font-lock-constant-face))

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
      ((std_function (function_name) @font-lock-preprocessor-face)))))

;;; Indentation

(defun spthy-ts-mode--logical-operator-indent-rule (node parent &rest _)
  (when (or (treesit-node-match-p
             node
             (spthy-ts-mode--regexp-opt-line
              "&" "∧" "|" "∨" "==>" "⇒" "<=>" "⇔" "not" "¬"))
            (treesit-node-match-p
             parent
             (spthy-ts-mode--regexp-opt-line
              "conjunction" "disjunction" "imp" "iff" "negation")))
    ;; Usually we just indent to the column of the parent, however,
    ;; if the parent is on the same line as its enclosing quantifier,
    ;; then we shift to the left as follows.
    ;; Instead of:
    ;; All a b c. A(a)
    ;;            ==> B(b, c)
    ;; We indent:
    ;; Ex a b c. A(a)
    ;;   ==> B(b, c)
    ;; More precisely, we indent exactly as if the parent were
    ;; on the line following the quantifier:
    ;; Ex a b c.
    ;;   A(a)
    ;;   ==> B(b, c)
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

(defun spthy-ts-mode--formula-writing-indent-rule (node parent bol &rest _)
  (when (or (and (null node)
                 (and-let* ((prev-node (spthy-ts-mode--prev-non-comment-node bol t)))
                   (member (treesit-node-type
                            (treesit-search-forward
                             prev-node
                             (lambda (nod)
                               (treesit-node-match-p
                                (treesit-node-parent nod) "^theory$"))
                             'backward))
                           '("lemma" "restriction" "case_test"
                             "accountability_lemma" "predicates"))))
            (and (treesit-node-match-p node "^)$")
                 (treesit-node-match-p parent "^nested_formula$")))
    (or (spthy-ts-mode--indent-try-insertions '("& A()" "A()" "a. A()") bol)
        `(,(funcall (alist-get 'prev-line treesit-simple-indent-presets)
                    node parent bol)
          . 0))))

(defun spthy-ts-mode--no-node-fallback-rule (node parent bol &rest _)
  ;; The formula-writing-indent-rule above might not have applied due to an
  ;; error, like a missing closing " in an incomplete formula of a lemma.
  ;; Hence we try again here.
  (when (and (null node)
             (spthy-ts-mode--prev-non-comment-node bol t))
    (spthy-ts-mode--indent-try-insertions
     '("& A()" "A()" "a. A()") bol)))

(defun spthy-ts-mode--incomplete-let-indent-rule (_node _parent bol &rest _)
  (when-let* ((node (spthy-ts-mode--prev-non-comment-node bol))
              (let-pos
               (when (equal (treesit-node-type node) "let")
                 (treesit-node-start node))))
    `(,let-pos . ,spthy-ts-mode-indent-offset)))

(defun spthy-ts-mode--indent-try-insertions (candidates bol)
  (let ((prefix (buffer-substring-no-properties (point-min) bol)))
    (with-temp-buffer
      (delay-mode-hooks (spthy-ts-mode))
      (insert prefix)
      (insert "\n\nend")
      (goto-char bol)
      (let ((temp-bol (point)))
        (catch 'result
          (dolist (str candidates)
            (insert str)
            (let* ((nod (treesit-node-at temp-bol))
                   (largest-nod (treesit--indent-largest-node-at temp-bol)))
              (when
                  ;; When we do not introduce an error...
                  (not (or (equal (treesit-node-type (treesit-node-parent nod))
                                  "ERROR")
                           ;; Error right before.
                           (equal (treesit-node-type
                                   (treesit-node-parent
                                    (treesit--indent-largest-node-at
                                     (treesit-node-start
                                      (spthy-ts-mode--prev-non-comment-node temp-bol)))))
                                  "ERROR")
                           ;; Error further in the inserted candidate string.
                           (treesit-search-forward
                            nod
                            (lambda (n)
                              (and
                               (> (treesit-node-start n) temp-bol)
                               ;; Not the end node we inserted.
                               (not (equal (treesit-node-end n) (point-max)))
                               (or (equal (treesit-node-type (treesit-node-parent n))
                                          "ERROR")
                                   (equal (treesit-node-type n)
                                          "ERROR")))))))
                (throw 'result
                       (treesit-simple-indent
                        largest-nod
                        (treesit-node-parent largest-nod)
                        temp-bol))))
            (delete-region temp-bol (point))))))))

(defun spthy-ts-mode--missing-closing-bracket-indent-rule
    (node _parent bol &rest _)
  (when (and (null node)
             (treesit-parent-until
              (spthy-ts-mode--prev-non-comment-node bol)
              (lambda (nod)
                (and (treesit-node-match-p (treesit-node-parent nod) "^ERROR$")
                     (not (treesit-node-match-p nod "^theory$"))))
              t))
    (spthy-ts-mode--indent-try-insertions '("]" "]->" ")" ">") bol)))

(defconst spthy-ts-mode--missing-query
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

(defalias 'spthy-ts-mode--proof-exists-p
  (spthy-ts-mode--throttled-query-function
   (treesit-query-compile 'spthy '((lemma proof_skeleton: (_) @proof)))
   t))

(defun spthy-ts-mode--within-proof-p (node &rest _)
  ;; The recursive check can be expensive, so first check that there even
  ;; exists a proof in the buffer at all.
  (when (spthy-ts-mode--proof-exists-p)
    (cl-flet ((proof-node-p (nod)
                (or (member (treesit-node-type nod)
                            '("cases" "case" "step"))
                    (member (treesit-node-field-name nod)
                            '("proof_skeleton")))))
      (treesit-parent-until
       node
       #'proof-node-p 'include-node))))

(defun spthy-ts-mode--non-comment-node-p (node)
  (not (member (treesit-node-type node) '("single_comment" "multi_comment"))))

(defun spthy-ts-mode--prev-non-comment-node (bol &optional prev-line)
  (let* ((node-at-bol (treesit-node-at bol))
         (node
          (if (and (< (treesit-node-start node-at-bol)
                      bol)
                   (spthy-ts-mode--non-comment-node-p node-at-bol))
              node-at-bol
            (save-excursion
              (goto-char bol)
              (treesit-search-forward-goto
               node-at-bol
               #'spthy-ts-mode--non-comment-node-p 'start 'backward 'all)
              (treesit-node-at (point))))))
    (unless
        (and prev-line
             ;; A non-nil PREV-LINE enforces that there
             ;; should be no blank line between the last
             ;; non-comment node and the line to indent.
             (save-excursion
               (goto-char bol)
               (cl-loop while
                        (and (= 0 (forward-line -1))
                             (>= (point) (treesit-node-start node)))
                        thereis (looking-at-p "[[:blank:]]*$"))))
      node)))

(defun spthy-ts-mode--prev-node-is (node-t &optional parent-t prev-line)
  (lambda (_node _parent bol &rest _)
    (let ((node (spthy-ts-mode--prev-non-comment-node bol prev-line)))
      (and
       (or (null node-t)
           (string-match-p
            node-t (or (treesit-node-type node) "")))
       (or (null parent-t)
           (string-match-p
            parent-t
            (treesit-node-type
             (treesit-node-parent node))))))))

(defun spthy-ts-mode--end-of-prev-node
    (_node _parent bol)
  (1- (treesit-node-end (spthy-ts-mode--prev-non-comment-node bol))))

;; Adapted from `standalone-parent' in `treesit-simple-indent-presets'.
(defun spthy-ts-mode--nested-or-standalone-parent (_node parent &rest _)
  (save-excursion
    (catch 'term
      (while parent
        (goto-char (treesit-node-start parent))
        (when (or (treesit-node-match-p
                   (treesit-node-parent parent)
                   (spthy-ts-mode--regexp-opt-line
                    "nested_process" "location_process" "replication"
                    "deterministic_choice" "nondeterministic_choice"))
                  (looking-back (rx bol (* whitespace))
                                (line-beginning-position)))
          (throw 'term (point)))
        (setq parent (treesit-node-parent parent))))))

(defun spthy-ts-mode--else-anchor (_node parent bol &rest _)
  (let* ((parent-bol (save-excursion
                       (goto-char (treesit-node-start parent))
                       (pos-bol)))
         (gp (treesit-node-parent parent))
         (else-node (car
                     (treesit-filter-child
                      gp
                      (lambda (c) (treesit-node-match-p c "^else$")))))
         (else-column
          (save-excursion
            (goto-char (treesit-node-start else-node))
            (current-column)))
         (gp-column
          (save-excursion
            (goto-char (treesit-node-start gp))
            (current-column))))
    (if (and
         (<= parent-bol (treesit-node-start else-node))
         (<= else-column gp-column))
        (treesit-node-start else-node)
      (treesit-node-start parent))))

(defconst spthy-ts-mode--process-nodes
  '("set_lock" "remove_lock" "input" "read_state" "delete_state"
    "set_state" "output" "event" "process_let" "binding"
    "conditional" "predefined_process" "inline_msr_process"
    "nested_process" "location_process" "deterministic_choice"
    "nondeterministic_choice" "replication"))

(defun spthy-ts-mode--regexp-opt-line (&rest strings)
  (concat "^" (regexp-opt (flatten-list strings)) "$"))

;; TODO embedded restrictions
(defvar spthy-ts-mode--indent-settings
  (cl-macrolet ((rxl (&rest args)
                  `(spthy-ts-mode--regexp-opt-line ,@args)))
    `((spthy
       ;; Don't interfere with proof formatting.
       (spthy-ts-mode--within-proof-p
        no-indent)

       ;; Comments.
       ((node-is ,(rxl "multi_comment" "single_comment"))
        no-indent)

       ;; Block comments (adapted from `c-ts-mode--simple-indent-rules').
       ;; `c-ts-common-looking-at-star' has to come before
       ;; `c-ts-common-comment-2nd-line-matcher'.
       ((and (parent-is "^multi_comment$") c-ts-common-looking-at-star)
        c-ts-common-comment-start-after-first-star -1)
       (,(lambda (_n parent &rest _)
           ;; Adapted from `c-ts-common-comment-2nd-line-matcher',
           ;; which has the type "comment" hard coded.
           (and (equal (treesit-node-type parent) "multi_comment")
                (save-excursion
                  (forward-line -1)
                  (back-to-indentation)
                  (eq (point) (treesit-node-start parent)))))
        c-ts-common-comment-2nd-line-anchor
        1)
       ((parent-is "^multi_comment$") prev-adaptive-prefix 0)

       ;; Formal comments.
       ((and no-node
             (parent-is "^formal_comment$")
             ,(lambda (_n _p bol &rest _)
                (save-excursion
                  (goto-char bol)
                  (looking-at-p "[[:blank:]]*$"))))
        prev-line 0)
       ((parent-is "^formal_comment$") no-indent)

       ;; Incomplete definitions.
       ((and no-node (spthy-ts-mode--prev-node-is ":" nil t))
        parent ,spthy-ts-mode-indent-offset)
       ;; Incomplete rules.
       ((spthy-ts-mode--prev-node-is ,(rxl "[" "--[") nil t)
        spthy-ts-mode--end-of-prev-node ,spthy-ts-mode-indent-offset)

       spthy-ts-mode--logical-operator-indent-rule
       spthy-ts-mode--formula-writing-indent-rule

       ;; Some incomplete cases where the parser
       ;; itself does not recover with a MISSING node.
       spthy-ts-mode--missing-closing-bracket-indent-rule
       ;; Handle cases where the parser does recover.
       spthy-ts-mode--parser-missing-node-indent-rule

       ((parent-is "^inline_msr_process$")
        parent 0)
       ((or (parent-is "^theory$")
            (parent-is "^tactic$"))
        column-0 0)
       ((and (or (n-p-gp "^]->$" "^action_fact$" nil)
                 (n-p-gp "^]$" ,(rxl "premise" "conclusion") nil))
             (spthy-ts-mode--prev-node-is "^,$" nil t))
        (nth-sibling 0 t) 0)
       ((n-p-gp "^]$" ,(rxl "premise" "conclusion") nil)
        parent 0)
       ((n-p-gp "^]->$" "^action_fact$" nil)
        parent 2)

       ((or
         (n-p-gp "^<=>$" "^predicate$" nil)
         (and no-node
              (spthy-ts-mode--prev-node-is "^<=>$" "^predicate$" t))
         (and (parent-is "^predicate$")
              (field-is "^formula$")))
        parent ,spthy-ts-mode-indent-offset)

       ((n-p-gp "^)$"
                ,(rxl "nested_term" "ifdef_nested"
                      "nested_process" "location_process")
                nil)
        parent 0)
       ;; TODO should be min of offset, parent ident length + 1? check grammar ) cases
       ((node-is "^)$")
        parent ,spthy-ts-mode-indent-offset)
       ((and (node-is "^]$")
             (spthy-ts-mode--prev-node-is "^,$"))
        (nth-sibling 0 t) 0)
       ((node-is
         ,(rxl "rule_attr" "function_attribute" "lemma_attr"
               "diff_lemma_attr" "restriction_attr" "language"))
        (nth-sibling 0 t) 0)
       ((and (parent-is "^quantified_formula$")
             (field-is "^variable$"))
        (nth-sibling 1) 0)
       ((and (parent-is "^quantified_formula$")
             (node-is "^\\.$"))
        parent 0)
       ((or (n-p-gp "^mset_term$"
                    ,(rxl "tuple_term")
                    nil)
            (n-p-gp ,(rxl "linear_fact" "persistent_fact")
                    ,(rxl "premise" "action_fact" "conclusion")
                    nil))
        (nth-sibling 0 t) 0)
       ;; TODO should be min of offset, grand-parent ident length + 1?
       ((n-p-gp "^mset_term$" "^arguments$" nil)
        grand-parent ,spthy-ts-mode-indent-offset)
       ((n-p-gp ,(rxl "mset_term" "term_eq" "fresh_var"
                      "linear_fact" "persistent_fact"
                      "equality")
                ,(rxl spthy-ts-mode--process-nodes)
                nil)
        parent ,spthy-ts-mode-indent-offset)
       ((or (parent-is "^quantified_formula$")
            (node-is "^arguments$")
            (node-is "^macro$"))
        parent ,spthy-ts-mode-indent-offset)
       ((or (n-p-gp "\""
                    ,(rxl "lemma" "restriction" "case_test"
                          "accountability_lemma")
                    nil)
            (node-is "^trace_quantifier$"))
        column-0 ,spthy-ts-mode-indent-offset)
       ((parent-is "^let$")
        parent ,spthy-ts-mode-indent-offset)
       ((n-p-gp ,(rxl "||" "|" "+")
                ,(rxl "deterministic_choice" "nondeterministic_choice")
                nil)
        parent 0)
       ((parent-is ,(rxl "nested_process" "location_process" "replication"))
        parent ,spthy-ts-mode-indent-offset)
       ((parent-is ,(rxl "deterministic_choice" "nondeterministic_choice"))
        parent ,spthy-ts-mode-indent-offset)

       ;; For "else if", "else lookup", anchor to else.
       ((and (parent-is ,(rxl "conditional" "read_state"))
             (node-is ,(rxl spthy-ts-mode--process-nodes))
             ,(lambda (_node parent &rest _)
                (equal (treesit-node-field-name parent) "else")))
        spthy-ts-mode--else-anchor ,spthy-ts-mode-indent-offset)
       ;; Nested else.
       ((and (parent-is ,(rxl "conditional" "read_state"))
             (node-is "else")
             ,(lambda (_node parent &rest _)
                (equal (treesit-node-field-name parent) "else")))
        spthy-ts-mode--else-anchor 0)
       ((and (parent-is ,(rxl "conditional" "read_state"))
             (node-is ,(rxl spthy-ts-mode--process-nodes)))
        parent ,spthy-ts-mode-indent-offset)

       ((node-is ,(rxl spthy-ts-mode--process-nodes))
        spthy-ts-mode--nested-or-standalone-parent 0)


       ((n-p-gp "^]$"
                ,(rxl "premise" "conclusion")
                nil)
        parent 0)
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
       ((match nil ,(rxl "premise" "conclusion")
               nil 1 1)
        parent 2)
       ;; Handle first action fact.
       ((match nil "^action_fact$"
               nil 1 1)
        parent 4)
       ((or (n-p-gp nil nil "^theory$")
            (parent-is "^simple_rule$")
            (n-p-gp nil nil "^tactic$")
            (parent-is "^rule_let_block$"))
        first-sibling ,spthy-ts-mode-indent-offset)
       ((n-p-gp "^,$" "^action_fact$" nil)
        parent 2)
       ((and no-node (or (parent-is "^premise$")
                         (parent-is "^conclusion$"))
             (spthy-ts-mode--prev-node-is "^,$" nil t))
        (nth-sibling 0 t) 0)
       ((and no-node (parent-is "^action_fact$")
             (spthy-ts-mode--prev-node-is "^,$" nil t))
        (nth-sibling 0 t) 0)
       ((and no-node (or (parent-is "^premise$")
                         (parent-is "^conclusion$")))
        parent 0)
       ((and no-node (parent-is "^action_fact$"))
        parent 2)
       spthy-ts-mode--no-node-fallback-rule
       (no-node prev-line 0)
       (catch-all parent 0)))))

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

    (c-ts-common-comment-setup)

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
           :commit "36d47f75150a028b53ce3ae8022994dc092d1e86")
   t)
  (when (treesit-ready-p 'spthy)
    (add-to-list 'auto-mode-alist '("\\.spthy\\'" . spthy-ts-mode))))

(provide 'spthy-ts-mode)

;;; spthy-ts-mode.el ends here
