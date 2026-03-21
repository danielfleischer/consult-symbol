;;; consult-symbol.el --- Consult-based symbol search with narrowing -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Daniel Fleischer

;; Author: Daniel Fleischer <danflscr@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (consult "2.0"))
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
   ((get sym 'cl--class) (cons ?t 'symbol))
   ((facep sym) (cons ?a 'face))
   ((get sym 'group-documentation) (cons ?G 'customize-group))
   ((commandp sym) (cons ?c 'command))
   ((and (fboundp sym) (macrop (symbol-function sym))) (cons ?m 'function))
   ((and (fboundp sym) (special-form-p (symbol-function sym))) (cons ?F 'function))
   ((and (boundp sym) (get sym 'standard-value)) (cons ?u 'variable))
   ((fboundp sym) (cons ?f 'function))
   ((boundp sym) (cons ?v 'variable))))

(defun consult-symbol--candidates ()
  "Collect all symbol candidates with type classification."
  (let (cands)
    (mapatoms
     (lambda (sym)
       (let ((name (symbol-name sym)))
         (unless (or (string-empty-p name) (string-prefix-p " " name))
           (when-let* ((type (consult-symbol--classify sym)))
             (push (propertize name
                               'consult--type (car type)
                               'multi-category (cons (cdr type) name))
                   cands))))))
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

;;;; Embark integration

(defun consult-symbol--embark-transformer (_type cand)
  "Transform `consult-symbol' candidate CAND to its specific embark type."
  (when-let* ((mc (get-text-property 0 'multi-category target)))
    (cons (cond
           ((commandp (car sym)) 'command)
           ((facep (car sym)) 'face)
           ((and (boundp (car sym)) (not (fboundp (car sym)))) 'variable)
           ((fboundp (car sym)) 'function)
           (t 'symbol))
          (cdr mc))))

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
                    :category 'multi-category
                    :require-match t
                    :sort t
                    :default default
                    :history consult-symbol-history
                    :lookup (lambda (sel &rest _) (intern-soft sel)))))
    (when selected
      (funcall consult-symbol-action selected))))

(provide 'consult-symbol)
;;; consult-symbol.el ends here
