;;; consult-symbol.el --- Consult-based symbol search with narrowing -*- lexical-binding: t; -*-

;; Author: Daniel Fleischer
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (consult "2.0") (marginalia "1.0"))
;; Keywords: convenience, matching
;; URL: https://github.com/danielfleischer/consult-symbol

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides `consult-symbol', an interactive command that searches all
;; Emacs symbols with narrowing by category (command, function,
;; variable, macro, face, etc.).  Built on top of the consult
;; completing-read framework.
;;
;; Suggested keybinding:
;;
;;   (keymap-global-set "C-c j" #'consult-symbol)

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

(defcustom consult-symbol-doc-width 80
  "Maximum width for docstring truncation in annotations."
  :type 'natnum)

(defcustom consult-symbol-value-width 30
  "Maximum width for variable value display in annotations."
  :type 'natnum)

(defcustom consult-symbol-face-sample "AaBbCc"
  "Sample text used for face preview in annotations."
  :type 'string)

(defcustom consult-symbol-include-internal t
  "Whether to include internal symbols (those containing \"--\")."
  :type 'boolean)

;;;; Narrowing categories

(defvar consult-symbol--types
  '((?c . "Command")
    (?f . "Function")
    (?m . "Macro")
    (?F . "Special Form")
    (?v . "Variable")
    (?u . "Custom Variable")
    (?a . "Face")
    (?G . "Custom Group")
    (?t . "CL Type"))
  "Alist of narrowing keys and category labels for symbol types.")

;;;; History

(defvar consult-symbol--history nil
  "History variable for `consult-symbol'.")

(add-to-list 'savehist-additional-variables 'consult-symbol--history)

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
Uses `customize-group' for custom groups, `describe-face' for faces,
`helpful-symbol' when available, and `describe-symbol' as fallback."
  (cond
   ((get sym 'group-documentation) (customize-group sym))
   ((facep sym) (describe-face sym))
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
           (cls (marginalia--symbol-class sym))
           (mid-fmt (format "%%-%ds" (+ consult-symbol-value-width 2))))
      (consult--annotate-align
       cand
       (concat cls
               (if (facep sym)
                   (propertize (format mid-fmt consult-symbol-face-sample) 'face sym)
                 (propertize (format mid-fmt (or val "")) 'face 'font-lock-constant-face))
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

(with-eval-after-load 'embark
  (setf (alist-get 'consult-symbol embark-transformer-alist)
        #'consult-symbol--embark-transformer))

;;;; Entry point

;;;###autoload
(defun consult-symbol ()
  "Search Emacs symbols with narrowing by category.
Symbols are grouped into commands, functions, macros, special forms,
variables, custom variables, faces, custom groups, and CL types."
  (interactive)
  (let* ((candidates (consult--slow-operation "Collecting symbols..."
                       (consult-symbol--candidates)))
         (selected (consult--read
                    candidates
                    :prompt "Symbol: "
                    :narrow (consult--type-narrow consult-symbol--types)
                    :group (consult--type-group consult-symbol--types)
                    :annotate #'consult-symbol--annotate
                    :category 'consult-symbol
                    :require-match t
                    :sort nil
                    :default (thing-at-point 'symbol)
                    :history 'consult-symbol--history
                    :lookup (lambda (sel cands &rest _) (intern-soft sel)))))
    (when selected
      (funcall consult-symbol-action selected))))

(provide 'consult-symbol)
;;; consult-symbol.el ends here
