;;; smartparens-spthy.el --- Additional configuration for spthy-ts-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ferdinand Nussbaum

;; Author: Ferdinand Nussbaum <ferdinand.nussbaum@inf.ethz.ch>
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
;; Smartparens configuration for spthy-ts-mode.

;;; Code:

;;; Code:
(require 'smartparens)

(defun sp-spthy-premise-or-conclusion-p ()
  (or
   (funcall (spthy-ts-mode--prev-node-is ":" :p "^simple_rule$")
            nil nil (- (point) 1))
   (and (funcall (spthy-ts-mode--prev-node-is ":" :p "^ERROR$")
                 nil nil (- (point) 1))
        (treesit-node-match-p
         (treesit-node-at
          (save-excursion
            (goto-char (treesit-node-start
                        (spthy-ts-mode--prev-non-comment-node (- (point) 1))))
            (pos-bol)))
         "^rule$"))
   (funcall (spthy-ts-mode--prev-node-is "]->" :p "^action_fact$")
            nil nil (- (point) 1))))

(defun sp-spthy-square-bracket-handler (_id _action _context)
  (when (sp-spthy-premise-or-conclusion-p)
    (insert " ")
    (save-excursion
      (insert " "))
    (indent-for-tab-command)))

(defun sp-spthy-action-fact-p (_id _action _context)
  (funcall (spthy-ts-mode--prev-node-is "]" :p "^premise$")
           nil nil (- (point) 3)))

(defun sp-spthy-tuple-p (_id _action _context)
  (let* ((node (treesit-node-at (1- (point))))
         (parent-type (treesit-node-type
                       (treesit-node-parent node))))
    (or (equal parent-type "tuple_term")
        (and (equal parent-type "ERROR")
             (treesit-parent-until
              node
              (lambda (nod)
                (treesit-node-match-p
                 nod (spthy-ts-mode--rxl
                      "mset_term" "arguments"
                      "linear_fact" "persistent_fact"))))))))

(sp-with-modes '(spthy-ts-mode)
  (sp-local-pair "--[" "]->"
                 :when '(sp-spthy-action-fact-p)
                 :post-handlers '(" || "))
  (sp-local-pair "[" "]"
                 :post-handlers
                 '(sp-spthy-square-bracket-handler))
  (sp-local-pair "<" ">"
                 :when '(sp-spthy-tuple-p)))

(provide 'smartparens-spthy)

;;; smartparens-spthy.el ends here
