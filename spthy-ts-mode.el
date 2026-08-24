;;; spthy-ts-mode.el --- tree-sitter support for the spthy language of the Tamarin prover  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ferdinand Nussbaum

;; Author: Ferdinand Nussbaum <ferdinand.nussbaum@inf.ethz.ch>
;; Version: 0.3.1
;; Package-Requires: ((emacs "31.0.90"))
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
;; A major mode for editing the security protocol theory language (spthy) of the
;; Tamarin prover (https://tamarin-prover.com), based on its tree-sitter
;; grammar.

;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'c-ts-common) ; for comment indentation and filling
(eval-when-compile (require 'rx))

;;;###autoload
(with-eval-after-load 'treesit
  (add-to-list
   'treesit-language-source-alist
   '(spthy "https://github.com/fnussbaum/tamarin-prover"
           :source-dir "tree-sitter/tree-sitter-spthy/src"
           :commit "599809bc1aa66e6d363df0c38eaa3e60a5547620")
   t)
  (when (treesit-ready-p 'spthy)
    (add-to-list 'auto-mode-alist '("\\.spthy\\'" . spthy-ts-mode))))

(defgroup spthy-ts nil
  "Major mode for editing Tamarin spthy files."
  :group 'languages)

(defcustom spthy-ts-indent-offset 2
  "Number of spaces for each indentation step in `spthy-ts-mode'."
  :type 'integer
  :safe 'integerp)

(defcustom spthy-ts-arrow-indent-offset -1
  "Indentation offset of --[]-> and --> in `spthy-ts-mode'.
The value, an integer, is relative to the indentation
of premises and conclusions.

When -1, the default, we indent like this:
rule Rule:
  []
 --[]->
  []

When -2, for example, the square brackets are aligned:
rule Rule:
  []
--[]->
  []
"
  :type 'integer
  :safe 'integerp)

(defcustom spthy-ts-formula-indent-style 'parent
  "Indentation style for formulas in `spthy-ts-mode'.
When `parent', the default, a binary operator or its second operand is
indented to align with the first operand. For example:

All a b c. A(a)
           ==> B(b) &
               C(c)

See also the option `spthy-ts-formula-align-operands'.

The `compact' style behaves like `parent', except when the first operand
starts on the same line as its enclosing quantifier. In that case, the
indentation is usually computed as if the first operand started on the
line after the quantifier. For example, instead of:

All a b c. A(a)
           ==> B(b, c)

`compact' indents as:

All a b c. A(a)
  ==> B(b, c)

which is the same as if the formula had been written:

All a b c.
  A(a)
  ==> B(b, c)

When `manual', indentation of logical operators and their operands is
left to the user and is not touched by commands like `indent-region'."
  :type '(choice (const parent) (const compact) (const manual)))

(defcustom spthy-ts-formula-align-operands nil
  "Whether to align operands in formulas in `spthy-ts-mode'.
When nil, the default, binary operators are indented to align with
their first operand, see also `spthy-ts-formula-indent-style'.
For example:

   A()
   ==> B()
       | C()
       | D()
         & E()

When non-nil, the indentation offset of binary operators is decreased
by 2. This aligns both operands of conjunctions and disjunctions, as
long as a single space follows the operator:

   A()
 ==> B()
   | C()
   | D()
   & E()

Note that the example demonstrates a case where the precedence of and
and or is not reflected in the indentation due to this option being
enabled."
  :type 'boolean
  :safe 'booleanp)

(defface spthy-ts-process-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face used for SAPIC+ keywords in spthy files.")

(defface spthy-ts-action-fact
  '((t (:inherit font-lock-property-name-face)))
  "Face used for action facts in spthy files.")

(defface spthy-ts-action-constraint
  '((t (:inherit font-lock-property-use-face)))
  "Face used for action facts within formulas in spthy files.")

(defface spthy-ts-built-in-fact
  '((t (:inherit font-lock-builtin-face)))
  "Face used for built-in facts in spthy files.")

(defface spthy-ts-built-in-function
  '((t (:inherit font-lock-builtin-face)))
  "Face used for built-in function calls in spthy files.")

(defface spthy-ts-function-call
  '((t (:inherit font-lock-function-call-face)))
  "Face used for function calls in spthy files.")

;; Adapted from `c-ts-mode-toggle-comment-style'.
(defun spthy-ts-toggle-comment-style (&optional arg)
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

(defvar spthy-ts--syntax-table
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
(defun spthy-ts--syntax-propertize (beg end)
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

(defconst spthy-ts--tokens
  '((general "theory" "begin" "end" "builtins" "functions" "export"
             "options" "equations" "predicates" "macros" "heuristic"
             "options" "tactic" "rule" "variants" "axiom" "restriction"
             "process" "lemma" "diffLemma" "all-traces" "exists-trace"
             "All" "Ex" (let "let") (rule_let_block "let")
             (rule_let_block "in") "fresh" "not" "test" "accounts"
             "account" "for" "equivLemma" "diffEquivLemma" "_restrict"
             ;; Proofs.
             "next" "case" "by" "ATTACK" ((solved)) ((mirrored)) "qed"
             "contradiction" "backward-search" "simplify"
             "induction" "rule-equivalence")
    (tactic "presort" "prio" "deprio" "smallest" "id")
    (preprocessor "#ifdef" "#else" "#endif" "#define" "#include")
    (quiet "modulo" "$" "~")
    (processes "out" (process_let "let") (process_let "in")
               (input "in") (read_state "in")
               "new" "lookup" "lock" "unlock" "delete" "insert"
               "event" "as" "if" "then" "else")
    ;; The "not" operator is highlighted like a keyword.
    (operators "&" "∧" "|" "∨" "==>" "⇒" "<=>" "⇔" "¬"
               "||" (replication "!") (non_deterministic_choice "+")
               "--[" "]->" "-->"))
   "Tamarin spthy tokens for tree-sitter font-locking.")

(defconst spthy-ts--builtin-functions
  '((hashing "h")
    (asymmetric-encryption "adec" "aenc" "pk")
    (signing "sign" "verify" "pk" "true")
    (revealing-signing "revealSign" "revealVerify" "getMessage" "pk" "true")
    (symmetric-encryption "senc" "sdec")
    ;; also includes "^" and "*" operators
    (diffie-hellman "inv" "1" "DH_neutral")
    (bilinear-pairing "inv" "1" "DH_neutral" "pmult" "em")
    (xor "zero")))

(eval-and-compile
  (defun spthy-ts--regexp-opt-line (&rest strings)
    (concat "^" (regexp-opt (flatten-list strings)) "$")))

(defmacro spthy-ts--rxl (&rest args)
  (apply #'spthy-ts--regexp-opt-line args))

(defconst spthy-ts--builtin-facts
  '("In" "Out" "Fr" "K" "KU" "KD"))

(defun spthy-ts--add-face-builtin-fact
    (node _override start end &rest _)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (when
        (and
         (<= start node-start node-end end)
         (member (treesit-node-text node) spthy-ts--builtin-facts))
      (add-face-text-property
       node-start node-end 'spthy-ts-built-in-fact))))

(defmacro spthy-ts--throttled-query-function (query collect)
  (let ((last-time (gensym "last-time"))
        (last-value (gensym "last-value")))
    `(let ((query ,query))
       (defvar-local ,last-time 0)
       (defvar-local ,last-value nil)
       (lambda ()
         (let ((current-time (time-convert (current-time) 'integer)))
           (if (> current-time
                  (+ ,last-time 2))
               (setq
                ,last-time current-time
                ,last-value
                (cl-loop
                 for (capture-name . node) in
                 (treesit-query-capture 'spthy query)
                 collect ,collect))
             ,last-value))))))

(defalias 'spthy-ts--imported-theories
  (spthy-ts--throttled-query-function
   (treesit-query-compile 'spthy '((built_in) @builtin))
   (progn
     ;; Silence byte-compiler warning about free variable.
     capture-name
     (treesit-node-type (treesit-node-child node 0)))))

(defun spthy-ts--add-face-builtin-function
    (node _override start end &rest _)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (when
        (and
         (<= start node-start node-end end)
         (let ((imported-theories (spthy-ts--imported-theories)))
           (cl-loop for (theory . idents) in spthy-ts--builtin-functions
                    thereis
                    (and (member (treesit-node-text node) idents)
                         (member (symbol-name theory) imported-theories)))))
      (add-face-text-property
       node-start node-end 'spthy-ts-built-in-function))))

(defalias 'spthy-ts--global-definitions
  (spthy-ts--throttled-query-function
   (treesit-query-compile
    'spthy '((let (mset_term) @process)
             (predicate predicate_identifier: (ident) @predicate)))
   (cons
    capture-name
    (treesit-node-text
     (treesit-node-at (treesit-node-start node))))))

(defun spthy-ts--add-face-process-identifier
    (node _override start end &rest _)
  (let* ((ident (treesit-node-at (treesit-node-start node)))
         (ident-start (treesit-node-start ident))
         (ident-end (treesit-node-end ident)))
    (when
        (and (<= start ident-start ident-end end)
             (or (treesit-node-match-p (treesit-node-parent node) "^let$")
                 (member `(process . ,(treesit-node-text ident))
                         (spthy-ts--global-definitions))))
      (add-face-text-property
       ident-start ident-end 'font-lock-variable-name-face))))

(defun spthy-ts--add-face-reference
    (node _override start end &rest _)
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node)))
    (when (and (<= start node-start node-end end)
               (member (cons 'predicate (treesit-node-text node))
                       (spthy-ts--global-definitions)))
      (add-face-text-property
       node-start node-end
       'font-lock-function-call-face))))

(defun spthy-ts--tokens-add-face (type face)
  (cons
   ;; For tokens defined as strings, build a vector and append the face.
   (list
    (apply
     #'vector
     (cl-remove-if #'consp (alist-get type spthy-ts--tokens)))
    face)
   ;; For tokens defined as lists, append the face to each one.
   (mapcar
    (lambda (token) (append token (list face)))
    (cl-remove-if-not
     #'consp
     (alist-get type spthy-ts--tokens)))))

(defvar spthy-ts--font-lock-settings
  (apply
   #'treesit-font-lock-rules
   `( :language spthy
      :feature comment
      (([(multi_comment) (single_comment)] @font-lock-comment-face)
       ( formal_comment comment_identifier: (ident) @font-lock-constant-face
         (:match ,(spthy-ts--rxl "section" "subsection" "text")
                 @font-lock-constant-face)))

      :language spthy
      :feature constant
      ((pub_name) @font-lock-string-face)

      :language spthy
      :feature operator
      (,@(spthy-ts--tokens-add-face
          'operators '@font-lock-operator-face))

      ;; Inspired by `spthy-mode'.
      :language spthy
      :feature quiet
      (,@(spthy-ts--tokens-add-face 'quiet '@font-lock-comment-face)
       (pub_var ["pub" ":"] @font-lock-comment-face)
       (fresh_var ["fresh" ":"] @font-lock-comment-face)
       (msg_var_or_nullary_fun ["msg" ":"] @font-lock-comment-face)
       (temporal_var ["node" ":"] @font-lock-comment-face)
       (nat_var ["nat" ":"] @font-lock-comment-face)
       (any_var ["ANY" ":"] @font-lock-comment-face))

      :language spthy
      :feature keyword
      (,@(spthy-ts--tokens-add-face
          'general '@font-lock-keyword-face)
       ,@(spthy-ts--tokens-add-face
          'processes '@spthy-ts-process-keyword)
       ,@(spthy-ts--tokens-add-face
          'preprocessor '@font-lock-preprocessor-face)
       ((atom) @font-lock-keyword-face)
       ("configuration" @font-lock-constant-face)
       ((option) @font-lock-builtin-face)
       ((built_in) @font-lock-builtin-face)
       (["step" "solve"] @font-lock-function-call-face)
       (["sorry"] @font-lock-warning-face))

      :language spthy
      :feature definition
      ((simple_rule rule_identifier: (ident) @font-lock-function-name-face)
       (restriction restriction_identifier: (ident) @font-lock-function-name-face)
       (lemma lemma_identifier: (ident) @font-lock-function-name-face)
       (diff_lemma lemma_identifier: (ident) @font-lock-function-name-face)
       (predicate predicate_identifier: (ident) @font-lock-function-name-face)
       (case_test test_identifier: (ident) @font-lock-variable-name-face)
       (accountability_lemma lemma_identifier: (ident) @font-lock-function-name-face)
       (tactic (ident) @font-lock-function-name-face)
       (let let_identifier: (mset_term) @spthy-ts--add-face-process-identifier))

      :language spthy
      :feature function
      ((nary_app function_identifier: (ident)
                 @spthy-ts-function-call)
       (binary_app function_identifier: (ident)
                   @spthy-ts-function-call))

      :language spthy
      :feature action-fact
      ((action_fact ( linear_fact fact_identifier: (ident)
                      @spthy-ts-action-fact))
       (action_fact ( persistent_fact fact_identifier: (ident)
                      @spthy-ts-action-fact))
       (event ( linear_fact fact_identifier: (ident)
                @spthy-ts-action-fact))
       (action_constraint ( linear_fact fact_identifier: (ident)
                            @spthy-ts-action-constraint)))

      ;; With this feature, references to predicates and predefined processes are
      ;; only highlighted when they are defined in the same file. In principle,
      ;; this could also be applied to function references or action fact
      ;; references in formulas, etc., but this kind of diagnostic functionality
      ;; is generally better left to the LSP server, for example. Here, one
      ;; downside is that the font locking is not necessarily automatically
      ;; updated when changing or adding a definition identifier.
      ;; `font-lock-update' can be manually invoked, of course.
      :language spthy
      :feature reference
      :override t
      (;; Predicate references. It might be annoying to always highlight action
       ;; facts as predicates before adding the @t annotation when writing lemmas.
       ;; Hence we look up the definitions and only highlight defined predicates.
       (predicate_ref predicate_identifier: (ident)
                      @spthy-ts--add-face-reference)
       (predicate predicate_identifier: (ident) @font-lock-function-name-face)
       ;; Process references.
       (predefined_process (mset_term) @spthy-ts--add-face-process-identifier)
       (let let_identifier: (mset_term) @spthy-ts--add-face-process-identifier))

      :language spthy
      :feature builtin-function
      :override t
      ((nary_app function_identifier: (ident)
                 @spthy-ts--add-face-builtin-function)
       (binary_app function_identifier: (ident)
                   @spthy-ts--add-face-builtin-function)
       (nullary_fun function_identifier: (ident)
                    @spthy-ts--add-face-builtin-function)
       ;; Could a nullary function potentially get shadowed
       ;; by a variable of the same name?
       ;; But then again, why would one choose a conflicting name...
       (msg_var_or_nullary_fun variable_identifier: (ident)
                               @spthy-ts--add-face-builtin-function))

      :language spthy
      :feature builtin-fact
      :override t
      ((action_constraint ( linear_fact fact_identifier: (ident)
                            @spthy-ts--add-face-builtin-fact))
       (action_constraint ( persistent_fact fact_identifier: (ident)
                            @spthy-ts--add-face-builtin-fact))
       (premise ( linear_fact fact_identifier: (ident)
                  @spthy-ts--add-face-builtin-fact))
       (premise ( persistent_fact fact_identifier: (ident)
                  @spthy-ts--add-face-builtin-fact))
       (conclusion ( linear_fact fact_identifier: (ident)
                     @spthy-ts--add-face-builtin-fact))
       (conclusion ( persistent_fact fact_identifier: (ident)
                     @spthy-ts--add-face-builtin-fact)))

      :language spthy
      :feature tactic
      (,@(spthy-ts--tokens-add-face
          'tactic '@font-lock-preprocessor-face)
       ((std_function (function_name) @font-lock-preprocessor-face))))))

;;; Indentation

(defun spthy-ts--logical-operator-indent-rule (node parent &rest _)
  (let* ((is-binary-op
          ;; We cannot use `treesit-node-match-p' here because it assumes
          ;; single-byte characters (see fast_c_string_match_internal). For
          ;; example, "∨" would match "(".
          (string-match-p
           (spthy-ts--rxl
            "&" "∧" "|" "∨" "==>" "⇒" "<=>" "⇔" )
           (or (treesit-node-type node) "")))
         (align-offset (if (and is-binary-op
                          spthy-ts-formula-align-operands)
                     -2
                   0)))
    (when (or is-binary-op
              (string-match-p
               (spthy-ts--rxl "not" "¬")
               (or (treesit-node-type node) ""))
              (and (not (null node))
                   (treesit-node-match-p
                    parent
                    (spthy-ts--rxl
                     "conjunction" "disjunction" "imp" "iff" "negation"))))
      (pcase spthy-ts-formula-indent-style
        ('manual
         '(no-indent))
        ('parent
         `(,(treesit-node-start parent) . ,align-offset))
        ('compact
         (cl-flet* ((above-parent-line-p (nod)
                      (< (treesit-node-start nod)
                         (save-excursion
                           (goto-char (treesit-node-start parent))
                           (pos-bol))))
                    (parent-quantifier-prev-line (nod)
                      (let ((nod-parent (treesit-node-parent nod)))
                        (or (equal (treesit-node-type nod-parent)
                                   "quantified_formula")
                            (above-parent-line-p nod-parent)))))
           (let* ((maybe-quantifier-child
                   (treesit-parent-until parent #'parent-quantifier-prev-line
                                         'include-node))
                  (maybe-quantifier
                   (treesit-node-parent maybe-quantifier-child))
                  (offset-to-quant-child
                   (- (treesit-node-start parent)
                      (treesit-node-start
                       maybe-quantifier-child))))
             (if (or
                  (above-parent-line-p maybe-quantifier)
                  ;; Do not shift to the left when the offset is large,
                  ;; as this could be confusing.
                  (>= offset-to-quant-child 2))
                 `(,(treesit-node-start parent) . ,align-offset)
               ;; Else shift.
               `(,(treesit-node-start maybe-quantifier) .
                 ,(+ spthy-ts-indent-offset
                     offset-to-quant-child
                     align-offset))))))))))

(defun spthy-ts--formula-writing-indent-rule (node parent bol &rest _)
  (when (or (and (null node)
                 (and-let* ((prev-node (spthy-ts--prev-non-comment-node bol t))
                            ((not (treesit-node-match-p prev-node "^,$")))
                            (target-node
                             (treesit-search-forward
                              prev-node
                              (lambda (nod)
                                (or
                                 (treesit-node-match-p nod "^embedded_restriction$")
                                 (treesit-node-match-p
                                  (treesit-node-parent nod)
                                  (spthy-ts--rxl "theory" "predicate"))))
                              'backward)))
                   (or (treesit-node-match-p
                        target-node
                        (spthy-ts--rxl
                         "lemma" "restriction" "case_test"
                         "accountability_lemma" "embedded_restriction"))
                       (and (treesit-node-match-p
                             (treesit-node-parent target-node) "^predicate$")
                            (equal
                             (treesit-node-field-name target-node)
                             "formula")))))
            (and (treesit-node-match-p node "^)$")
                 (treesit-node-match-p
                  parent
                  (spthy-ts--rxl
                   "nested_formula" "embedded_restriction"))))
    (or
     (let ((spthy-ts-formula-indent-style
            (if (eq spthy-ts-formula-indent-style 'manual)
                'parent
              spthy-ts-formula-indent-style)))
       (spthy-ts--indent-try-insertions
        '("| A()" "A()" "| A()\"" "A()\"" "a. A()" "a. A()\"")
        bol))
     `(,(funcall (alist-get 'prev-line treesit-simple-indent-presets)
                 node parent bol)
       . 0))))

(defun spthy-ts--no-node-fallback-rule (node _parent bol &rest _)
  ;; The formula-writing-indent-rule above might not have applied due to an
  ;; error, like a missing closing " in an incomplete formula of a lemma.
  ;; Hence we try again here.
  (when (and (null node)
             (spthy-ts--prev-non-comment-node bol t))
    (let ((spthy-ts-formula-indent-style
           (if (eq spthy-ts-formula-indent-style 'manual)
               'parent
             spthy-ts-formula-indent-style)))
      (spthy-ts--indent-try-insertions
       '("| A()" "A()" "| A()\"" "A()\"" "a. A()" "a. A()\"")
       bol))))

(defun spthy-ts--incomplete-let-indent-rule (_node _parent bol &rest _)
  (when-let* ((node (spthy-ts--prev-non-comment-node bol))
              (let-pos
               (when (equal (treesit-node-type node) "let")
                 (treesit-node-start node))))
    `(,let-pos . ,spthy-ts-indent-offset)))

(defun spthy-ts--indent-try-insertions (candidates bol)
  (let ((prefix (buffer-substring-no-properties (point-min) bol))
        ;; The buffer-local values could differ from the default.
        (formula-indent-style spthy-ts-formula-indent-style)
        (formula-align-operands spthy-ts-formula-align-operands)
        (indent-offset spthy-ts-indent-offset)
        (arrow-indent-offset spthy-ts-arrow-indent-offset))
    (with-work-buffer
      (delay-mode-hooks (spthy-ts-mode))
      (setq-local spthy-ts-formula-indent-style formula-indent-style)
      (setq-local spthy-ts-formula-align-operands formula-align-operands)
      (setq-local spthy-ts-indent-offset indent-offset)
      (setq-local spthy-ts-arrow-indent-offset arrow-indent-offset)
      (insert prefix)
      (insert "\n\nend")
      (goto-char bol)
      (let ((temp-bol (point)))
        (catch 'result
          (dolist (str candidates)
            (insert str)
            (let* ((largest-nod (treesit--indent-largest-node-at temp-bol)))
              (when
                  (not
                   ;; Check for errors in and right before the insertion.
                   (let ((pos (treesit-node-start
                               (spthy-ts--prev-non-comment-node
                                (pos-bol 2))))
                         (prev-pos
                          (treesit-node-start
                           (spthy-ts--prev-non-comment-node temp-bol))))
                     (catch 'error
                       (while (>= pos prev-pos)
                         (when (treesit-node-check
                                (treesit-node-parent
                                 (treesit--indent-largest-node-at pos))
                                'has-error)
                           (throw 'error t))
                         (decf pos)))))
                (throw 'result
                       (treesit-simple-indent
                        largest-nod
                        (treesit-node-parent largest-nod)
                        temp-bol))))
            (delete-region temp-bol (point))))))))

(defun spthy-ts--missing-closing-bracket-indent-rule
    (node _parent bol &rest _)
  (when (and (null node)
             (treesit-parent-until
              (spthy-ts--prev-non-comment-node bol)
              (lambda (nod)
                (and (treesit-node-match-p (treesit-node-parent nod) "^ERROR$")
                     (not (treesit-node-match-p nod "^theory$"))))
              t))
    (spthy-ts--indent-try-insertions '("]" "]->" ")" ">") bol)))

(defconst spthy-ts--missing-query
  (treesit-query-compile 'spthy '((MISSING) @missing)))

(defun spthy-ts--parser-missing-node-indent-rule
    (node _parent bol &rest _)
  (when-let* ((_ (null node))
              (missing-node
               (car
                (let ((pos (treesit-node-end
                            (spthy-ts--prev-non-comment-node bol))))
                  (member-if
                   (lambda (nod)
                     (equal (treesit-node-start nod) pos))
                   (treesit-query-capture
                    'spthy spthy-ts--missing-query
                    nil nil 'node-only))))))
    (spthy-ts--indent-try-insertions
     (list (treesit-node-type missing-node)) bol)))

(defalias 'spthy-ts--proof-exists-p
  (spthy-ts--throttled-query-function
   (treesit-query-compile 'spthy '((lemma proof_skeleton: (_) @proof)))
   ;; Silence byte-compiler warning about free variable.
   capture-name))

(defun spthy-ts--within-proof-p (node &rest _)
  ;; The recursive check can be expensive, so first check that there even
  ;; exists a proof in the buffer at all.
  (when (spthy-ts--proof-exists-p)
    (cl-flet ((proof-node-p (nod)
                (or (member (treesit-node-type nod)
                            '("cases" "case" "step"))
                    (member (treesit-node-field-name nod)
                            '("proof_skeleton")))))
      (treesit-parent-until
       node
       #'proof-node-p 'include-node))))

(defun spthy-ts--non-comment-node-p (node)
  (not (member (treesit-node-type node) '("single_comment" "multi_comment"))))

(defun spthy-ts--prev-non-comment-node (bol &optional prev-line)
  (let* ((node-at-bol (treesit-node-at bol))
         (node
          (if (and (< (treesit-node-start node-at-bol)
                      bol)
                   (spthy-ts--non-comment-node-p node-at-bol))
              node-at-bol
            (save-excursion
              (goto-char bol)
              (treesit-search-forward-goto
               node-at-bol
               #'spthy-ts--non-comment-node-p 'start 'backward 'all)
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

(cl-defun spthy-ts--prev-node-is (node-t &key p gp prev-line bolp)
  (lambda (_node _parent bol &rest _)
    (let ((node (if bolp
                    (treesit--indent-prev-line-node bol)
                  (spthy-ts--prev-non-comment-node bol prev-line))))
      (and
       (or (null node-t)
           (string-match-p
            node-t (or (treesit-node-type node) "")))
       (or (null p)
           (string-match-p
            p
            (or (treesit-node-type
                 (treesit-node-parent node))
                "")))
       (or (null gp)
           (string-match-p
            gp
            (or (treesit-node-type
                 (treesit-node-parent
                  (treesit-node-parent node)))
                "")))))))

(defun spthy-ts--end-of-prev-node
    (_node _parent bol)
  (1- (treesit-node-end (spthy-ts--prev-non-comment-node bol))))

;; Adapted from `standalone-parent' in `treesit-simple-indent-presets'.
(defun spthy-ts--nested-or-standalone-parent (_node parent &rest _)
  (save-excursion
    (catch 'term
      (while parent
        (goto-char (treesit-node-start parent))
        (when (or (treesit-node-match-p
                   (treesit-node-parent parent)
                   (spthy-ts--rxl
                    "nested_process" "location_process" "replication"
                    "deterministic_choice" "nondeterministic_choice"))
                  (looking-back (rx bol (* whitespace))
                                (line-beginning-position)))
          (throw 'term (point)))
        (setq parent (treesit-node-parent parent))))))

(defun spthy-ts--else-anchor (_node parent &rest _)
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

(defun spthy-ts--matching-bracket-start (node parent &rest _)
  (let ((matching-bracket
         (pcase (treesit-node-type node)
           (")" "^($")
           ("}" "^{$")
           ("]" "^\\[$"))))
    (treesit-node-start
     (or (progn
           (while (and node (not (treesit-node-match-p node matching-bracket)))
             (setq node (treesit-node-prev-sibling node)))
           node)
         parent))))

(eval-and-compile
  (defconst spthy-ts--process-nodes
    '("set_lock" "remove_lock" "input" "read_state" "delete_state"
      "set_state" "output" "event" "process_let" "binding"
      "conditional" "predefined_process" "inline_msr_process"
      "nested_process" "location_process" "deterministic_choice"
      "non_deterministic_choice" "replication"
      "single_comment" "multi_comment"))
  (defconst spthy-ts--signature-spec-nodes
    '("built_ins" "functions" "equations"
      "predicates" "macros" "options")))

(defvar spthy-ts--indent-rules
  (cl-macrolet ((rxl (&rest args)
                  `(spthy-ts--regexp-opt-line ,@args))
                (prev-node-is (&rest args)
                  `(spthy-ts--prev-node-is ,@args))
                ;; For debugging: Do not inline lambda.
                ;; (prev-node-is (&rest args)
                ;;   `(list 'spthy-ts--prev-node-is ,@args))
                )
    `((spthy
       ;; Don't interfere with proof formatting.
       (spthy-ts--within-proof-p
        no-indent)

       ;;; Comments.
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


       ;;; Incomplete definitions.
       ((and no-node ,(prev-node-is ":" :prev-line t))
        column-0 spthy-ts-indent-offset)
       ;; Incomplete rules.
       (,(prev-node-is (rxl "[" "--[") :prev-line t)
        spthy-ts--end-of-prev-node 2)


       ;;; Lemmas and formulas.
       spthy-ts--logical-operator-indent-rule
       spthy-ts--formula-writing-indent-rule
       ((and (parent-is "^quantified_formula$")
             (field-is "^variable$"))
        (nth-sibling 1) 0)
       ((and (parent-is "^quantified_formula$")
             (node-is "^\\.$"))
        parent 0)
       ((parent-is "^quantified_formula$")
        parent spthy-ts-indent-offset)
       ((field-is "^formula$")
        parent spthy-ts-indent-offset)
       ((and (not no-node)
             (parent-is ,(rxl "lemma" "restriction" "case_test"
                              "accountability_lemma")))
        column-0 spthy-ts-indent-offset)


       ;;; Signature specifications: builtins, functions, equations,
       ;;; predicates, macros, options
       ((and no-node
             ,(prev-node-is
               "^,$"
               :p (rxl spthy-ts--signature-spec-nodes)
               :prev-line t))
        column-0 spthy-ts-indent-offset)
       ((or
         (n-p-gp ,(rxl "=" "<=>") ,(rxl "macro" "equation" "predicate") nil)
         (and no-node
              ,(prev-node-is
                (rxl "=" "<=>") :p (rxl "macro" "equation" "predicate")
                :prev-line t))
         (and (parent-is "^macro$")
              (field-is "^term$"))
         (and (parent-is "^equation$")
              (field-is "^right$"))
         (and (parent-is "^predicate$")
              (field-is "^formula$")))
        parent spthy-ts-indent-offset)
       ((or (n-p-gp "^,$" ,(rxl spthy-ts--signature-spec-nodes) nil)
            ;; To support "comma on new line" style.
            (and no-node
                 ,(prev-node-is
                   nil
                   :p (rxl spthy-ts--signature-spec-nodes)
                   :bolp t)))
        column-0 ,(lambda (&rest _)
                    (- spthy-ts-indent-offset 2)))
       ((parent-is ,(rxl spthy-ts--signature-spec-nodes))
        parent spthy-ts-indent-offset)


       ;; Handle missing brackets where the parser
       ;; itself does not recover with a MISSING node.
       spthy-ts--missing-closing-bracket-indent-rule
       ;; Handle cases where the parser does recover.
       spthy-ts--parser-missing-node-indent-rule


       ;;; Top-level.
       ((and (not no-node) (parent-is "^theory$"))
        column-0 0)


       ;;; Tactics.
       ((and no-node ,(prev-node-is nil :p (rxl "prio" "deprio") :prev-line t :bolp t))
        prev-line 0)
       ((parent-is "^tactic$")
        column-0 0)
       (,(prev-node-is (rxl "prio" "deprio") :prev-line t :bolp t)
        column-0 spthy-ts-indent-offset)
       ((n-p-gp nil nil "^tactic$")
        prev-sibling 0)


       ((node-is ,(rxl ")" "}"))
        spthy-ts--matching-bracket-start 0)


       ;; TODO Does process writing need improvements?
       ;;; Processes.
       ;; The inline_msr_process rule has to come before
       ;; the rule rules below.
       ((parent-is "^inline_msr_process$")
        parent 0)
       ((parent-is "^let$")
        parent spthy-ts-indent-offset)
       ((n-p-gp ,(rxl "||" "|" "+")
                ,(rxl "deterministic_choice" "nondeterministic_choice")
                nil)
        parent 0)
       ((parent-is ,(rxl "nested_process" "location_process" "replication"))
        parent spthy-ts-indent-offset)
       ((parent-is ,(rxl "deterministic_choice" "nondeterministic_choice"))
        parent spthy-ts-indent-offset)

       ;; For "else if", "else lookup", anchor to else.
       ((and (parent-is ,(rxl "conditional" "read_state"))
             (node-is ,(rxl spthy-ts--process-nodes))
             ,(lambda (_node parent &rest _)
                (equal (treesit-node-field-name parent) "else")))
        spthy-ts--else-anchor spthy-ts-indent-offset)
       ;; Nested else.
       ((and (parent-is ,(rxl "conditional" "read_state"))
             (node-is "else")
             ,(lambda (_node parent &rest _)
                (equal (treesit-node-field-name parent) "else")))
        spthy-ts--else-anchor 0)
       ((and (parent-is ,(rxl "conditional" "read_state"))
             (node-is ,(rxl spthy-ts--process-nodes)))
        parent spthy-ts-indent-offset)

       ((n-p-gp ,(rxl "mset_term" "term_eq" "fresh_var"
                      "linear_fact" "persistent_fact"
                      "equality")
                ,(rxl spthy-ts--process-nodes)
                nil)
        parent spthy-ts-indent-offset)
       ((node-is ,(rxl spthy-ts--process-nodes))
        spthy-ts--nested-or-standalone-parent 0)


       ;;; Rules.
       ((and (or (n-p-gp "^]->$" "^action_fact$" nil)
                 (n-p-gp "^]$" ,(rxl "premise" "conclusion") nil))
             ,(prev-node-is "^,$" :prev-line t))
        (nth-sibling 0 t) 0)
       ((n-p-gp "^]$" ,(rxl "premise" "conclusion") nil)
        parent 0)
       ((n-p-gp "^]->$" "^action_fact$" nil)
        parent 2)
       ((node-is "^rule_let_block$")
        column-0 spthy-ts-indent-offset)
       ((n-p-gp "^in$" "^rule_let_block$" nil)
        parent 0)
       ((or (node-is "^-->$")
            (node-is "^action_fact$")
            ;; Handle incomplete rules.
            ,(prev-node-is "^\\]$" :p "^premise$"))
        column-0
        ,(lambda (&rest _)
           (max
            0
            (+ spthy-ts-indent-offset
               spthy-ts-arrow-indent-offset))))
       ((or (node-is "^premise$")
            (node-is "^conclusion$")
            ,(prev-node-is "^\\]->$" :p "^action_fact$")
            ,(prev-node-is "^-->$"))
        column-0 spthy-ts-indent-offset)
       spthy-ts--incomplete-let-indent-rule
       ((parent-is "^rule_let_block$")
        (nth-sibling 0 t) 0)
       ;; Handle first fact within rule premises and conclusions.
       ((match nil ,(rxl "premise" "conclusion")
               nil 1 1)
        parent 2)
       ;; Handle first action fact.
       ((match nil "^action_fact$"
               nil 1 1)
        parent 4)
       ((n-p-gp "^,$" "^action_fact$" nil)
        parent 2)


       ;;; Misc
       ((and (node-is "^\\]$")
             ,(prev-node-is "^,$"))
        (nth-sibling 0 t) 0)
       ((node-is "^]$")
        spthy-ts--matching-bracket-start 0)
       ((node-is
         ,(rxl "rule_attr" "function_attribute" "lemma_attr"
               "diff_lemma_attr" "restriction_attr" "language"))
        (nth-sibling 0 t) 0)
       ((query ((tuple_term left: (mset_term) @term)))
        parent 1)
       ((or (n-p-gp "^mset_term$"
                    ,(rxl "tuple_term")
                    nil)
            (n-p-gp ,(rxl "linear_fact" "persistent_fact")
                    ,(rxl "premise" "action_fact" "conclusion")
                    nil))
        (nth-sibling 0 t) 0)
       ((node-is "^arguments$")
        parent spthy-ts-indent-offset)

       ;;; Some empty line and fallback cases.
       ((and no-node (or (parent-is "^premise$")
                         (parent-is "^conclusion$"))
             ,(prev-node-is "^,$" :prev-line t))
        (nth-sibling 0 t) 0)
       ((and no-node (parent-is "^action_fact$")
             ,(prev-node-is "^,$" :prev-line t))
        (nth-sibling 0 t) 0)
       ((and no-node (or (parent-is "^premise$")
                         (parent-is "^conclusion$")))
        parent 0)
       ((and no-node (parent-is "^action_fact$"))
        parent 2)
       spthy-ts--no-node-fallback-rule
       ((node-is ,(rxl "multi_comment" "single_comment"))
        parent 0)
       (no-node prev-line 0)
       (catch-all parent 0)))))

;;; Things

(defun spthy-ts--node-defun-p (node)
  (and (equal (treesit-node-type (treesit-node-parent node)) "theory")
       (not (member (treesit-node-type node)
                    '( nil "single_comment" "multi_comment"
                       "theory" "begin" "end" "ident")))))

(defvar spthy-ts--treesit-things
  '((spthy
     (defun spthy-ts--node-defun-p))))

;;; Imenu

(defun spthy-ts--defun-name (node)
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

(defun spthy-ts--parent-is-theory-p (node)
  (treesit-node-match-p (treesit-node-parent node) "^theory$"))

(defvar spthy-ts--imenu-settings
  `(( "Process" ,(spthy-ts--rxl "process" "let")
      spthy-ts--parent-is-theory-p nil)
    ( "Rule" "^rule$"
      spthy-ts--parent-is-theory-p nil)
    ( "Restriction" "^restriction$"
      spthy-ts--parent-is-theory-p nil)
    ( "Lemma" "^lemma$"
      spthy-ts--parent-is-theory-p nil)
    ( "Case Test" "^case_test$"
      spthy-ts--parent-is-theory-p nil)
    ( "Accountability Lemma" "^accountability_lemma$"
      spthy-ts--parent-is-theory-p nil)
    ( "diffLemma" "^diff_lemma$"
      spthy-ts--parent-is-theory-p nil)))

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
  :syntax-table spthy-ts--syntax-table

  (when (treesit-ready-p 'spthy)
    (treesit-parser-create 'spthy)

    (setq-local treesit-font-lock-settings spthy-ts--font-lock-settings)
    (setq-local syntax-propertize-function #'spthy-ts--syntax-propertize)

    (setq-local treesit-simple-indent-rules spthy-ts--indent-rules)

    (setq-local treesit-defun-name-function #'spthy-ts--defun-name)
    (setq-local treesit-defun-tactic 'top-level)
    (setq-local treesit-simple-imenu-settings spthy-ts--imenu-settings)
    (setq-local treesit-thing-settings spthy-ts--treesit-things)

    (c-ts-common-comment-setup)

    (when (boundp 'electric-pair-pairs)
      (setq-local electric-pair-pairs
                  (cons
                   '(("--\\[" . "]->"))
                   electric-pair-pairs)))

    (setq-local treesit-font-lock-feature-list
                '(( comment constant quiet)
                  ( keyword)
                  ( action-fact reference builtin-fact
                    builtin-function operator tactic)
                  ( definition function)))

    (treesit-major-mode-setup)))

(provide 'spthy-ts-mode)

;; Local Variables:
;; byte-compile-warnings: (not make-local)
;; End:

;;; spthy-ts-mode.el ends here
