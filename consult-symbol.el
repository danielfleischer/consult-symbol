;;; consult-symbol.el --- Consult-based symbol search with narrowing -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Daniel Fleischer

;; Author: Daniel Fleischer <danflscr@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (consult "2.0") (marginalia "1.0"))
;; Keywords: convenience, matching
;; URL: https://github.com/danielfleischer/consult-symbol

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `consult-symbol', an interactive command that searches all
;; Emacs symbols with narrowing by category (command, function,
;; variable, macro, face, etc.).  Built on top of the consult
;; completing-read framework.

;; For embark integration, run `M-x consult-symbol-setup-embark'.

;;; Code:

(require 'consult)
(require 'marginalia)

;;;; Customization

(defgroup consult-symbol nil
  "Consult-based symbol search."
  :group 'consult
  :prefix "consult-symbol-")

(defcustom consult-symbol-action #'consult-symbol--default-action
  "Action function called with the selected symbol.
The function receives a symbol as its argument."
  :type 'function)

(defcustom consult-symbol-history 'consult-symbol--history
  "History variable to use for `consult-symbol'.
If nil, use the default minibuffer history.
If a symbol, use that symbol as the history variable."
  :type '(choice (const :tag "Default minibuffer history" nil)
                 (const :tag "Dedicated consult-symbol history" consult-symbol--history)
                 (symbol :tag "Custom history variable")))

(defcustom consult-symbol-doc-width 80
  "Maximum width for docstring truncation in annotations."
  :type 'integer)

(defcustom consult-symbol-value-width 30
  "Maximum width for variable value display in annotations."
  :type 'integer)

(defcustom consult-symbol-face-sample "AaBbCc"
  "Sample text used for face preview in annotations."
  :type 'string)

(defcustom consult-symbol-include-internal t
  "Whether to include internal symbols (those containing \"--\")."
  :type 'boolean)

(defcustom consult-symbol-types
  '((?c . "Command")
    (?f . "Function")
    (?m . "Macro")
    (?F . "Special Form")
    (?v . "Variable")
    (?u . "Custom Variable")
    (?a . "Face")
    (?G . "Custom Group")
    (?t . "CL Type"))
  "Alist of narrowing keys and category labels for symbol types.
Each entry is (CHARACTER . LABEL).  The character is used as the
narrowing key and the label is displayed in the group header."
  :type '(alist :key-type character :value-type string))

;;;; History

(defvar consult-symbol--history nil
  "History variable for `consult-symbol'.")

(unless (or (not (bound-and-true-p savehist-mode))
            (memq 'consult-symbol--history (bound-and-true-p savehist-ignored-variables)))
  (defvar savehist-minibuffer-history-variables)
  (add-to-list 'savehist-minibuffer-history-variables 'consult-symbol--history))

;;;; Candidate collection

(defun consult-symbol--classify (sym)
  "Classify symbol SYM, returning a narrowing key character or nil.
Maps the primary class to one of the narrowing categories."
  (cond
   ((get sym 'cl--class) ?t)
   ((facep sym) ?a)
   ((get sym 'group-documentation) ?G)
   ((commandp sym) ?c)
   ((and (fboundp sym) (macrop (symbol-function sym))) ?m)
   ((and (fboundp sym) (special-form-p (symbol-function sym))) ?F)
   ((and (boundp sym) (get sym 'standard-value)) ?u)
   ((fboundp sym) ?f)
   ((boundp sym) ?v)))

(defun consult-symbol--candidates ()
  "Collect all symbol candidates with type classification."
  (let (cands)
    (mapatoms
     (lambda (sym)
       (let ((name (symbol-name sym)))
         (unless (or (string-empty-p name)
                     (string-prefix-p " " name)
                     (and (not consult-symbol-include-internal)
                          (string-match-p "--" name)))
           (when-let* ((type (consult-symbol--classify sym)))
             (push (propertize name 'consult--type type) cands))))))
    (nreverse cands)))

;;;; Default action

(defun consult-symbol--default-action (sym)
  "Default action for selected symbol SYM.
Uses `customize-group' for pure custom groups, `describe-face' for
pure faces, `helpful-symbol' when available, and `describe-symbol'
as fallback."
  (cond
   ((and (get sym 'group-documentation) (not (fboundp sym)) (not (boundp sym)))
    (customize-group sym))
   ((and (facep sym) (not (fboundp sym)) (not (boundp sym)))
    (describe-face sym))
   ((fboundp 'helpful-symbol) (helpful-symbol sym))
   (t (describe-symbol sym))))

;;;; Annotation

(defun consult-symbol--doc (sym)
  "Get the first line of documentation for symbol SYM."
  (when-let* ((doc (cond
                    ((fboundp sym)
                     (ignore-errors (documentation sym t)))
                    ((facep sym) (face-documentation sym))
                    ((boundp sym)
                     (documentation-property sym 'variable-documentation t))
                    ((get sym 'group-documentation)))))
    (let ((line (car (split-string doc "\n"))))
      (if (> (length line) consult-symbol-doc-width)
          (concat (substring line 0 (- consult-symbol-doc-width 3)) "...")
        line))))

(defun consult-symbol--value-string (sym)
  "Return a truncated printed representation of SYM's value."
  (when (boundp sym)
    (let ((val (ignore-errors (prin1-to-string (symbol-value sym)))))
      (when val
        (if (> (length val) consult-symbol-value-width)
            (concat (substring val 0 (- consult-symbol-value-width 3)) "...")
          val)))))

(defun consult-symbol--annotate (cand)
  "Annotate symbol candidate CAND with class, value, and docstring."
  (when-let* ((sym (intern-soft cand)))
    (let* ((doc (consult-symbol--doc sym))
           (val (consult-symbol--value-string sym))
           (location (when (fboundp sym)
                       (string-truncate-left
                        (abbreviate-file-name (cdr (find-function-library sym)))
                        consult-symbol-value-width)))
           (cls (marginalia--symbol-class sym))
           (mid-fmt (format "%%-%ds" (+ consult-symbol-value-width 2))))
      (consult--annotate-align
       cand
       (concat cls
               (if (facep sym)
                   (propertize (format mid-fmt consult-symbol-face-sample) 'face sym)
                 (propertize (format mid-fmt (or val location "")) 'face 'font-lock-constant-face))
               (when doc
                 (propertize doc 'face 'shadow)))))))

;;;; Embark integration

(defun consult-symbol--embark-transformer (_type cand)
  "Transform `consult-symbol' candidate CAND to its specific embark type."
  (when-let* ((sym (intern-soft cand)))
    (cons (cond
           ((commandp sym) 'command)
           ((facep sym) 'face)
           ((and (boundp sym) (not (fboundp sym))) 'variable)
           ((fboundp sym) 'function)
           (t 'symbol))
          cand)))

(defvar embark-transformer-alist)

(defun consult-symbol-setup-embark ()
  "Enable embark integration."
  (interactive)
  (setf (alist-get 'consult-symbol embark-transformer-alist)
        #'consult-symbol--embark-transformer))

;;;; Entry point

;;;###autoload
(defun consult-symbol ()
  "Search Emacs symbols with narrowing by category.
Symbols are grouped into commands, functions, macros, special forms,
variables, custom variables, faces, custom groups, and CL types."
  (interactive)
  (let* ((default (thing-at-point 'symbol))
         (prompt (if default
                     (format "Symbol (default %s): " default)
                   "Symbol: "))
         (candidates (consult--slow-operation "Collecting symbols..."
                       (consult-symbol--candidates)))
         (selected (consult--read
                    candidates
                    :prompt prompt
                    :narrow (consult--type-narrow consult-symbol-types)
                    :group (consult--type-group consult-symbol-types)
                    :annotate #'consult-symbol--annotate
                    :category 'consult-symbol
                    :require-match t
                    :sort t
                    :default default
                    :history consult-symbol-history
                    :lookup (lambda (sel _ &rest _) (intern-soft sel)))))
    (when selected
      (funcall consult-symbol-action selected))))

(provide 'consult-symbol)
;;; consult-symbol.el ends here
