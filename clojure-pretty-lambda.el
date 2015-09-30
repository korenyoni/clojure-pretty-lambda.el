;; modified by Yonatan Koren to work for Clojure's `fn' keyword
;; contact: yon@yonatankoren.com
;;
;;; clojure-pretty-lambda.el --- Show the word `fn' as the Greek letter lambda.
;;
;; ORIGINAL DESCRIPTION:
;; Filename: pretty-lambdada.el
;; Description: Show the word `lambda' as the Greek letter.
;; Author: Drew Adams
;;         See http://www.emacswiki.org/emacs/PrettyLambda for the original
;;         code snippet and its history.
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2009-2015, Drew Adams, all rights reserved.
;; Created: Sun Jun 14 11:07:04 2009 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 11:10:11 2015 (-0800)
;;           By: dradams
;;     Update #: 152
;; URL: http://www.emacswiki.org/pretty-lambdada.el
;; Doc URL: http://www.emacswiki.org/PrettyLambda
;; Keywords: convenience display
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OMMITED SPECIFIC INFO FOR pretty-lambdada.el 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defgroup clojure-pretty-lambda nil
  "Display of the word `fn' as the Greek character lambda."
    :group 'convenience :group 'programming)

;;;###autoload
(defcustom clojure-pretty-lambda-auto-modes
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "*Modes affected by `clojure-pretty-lambda-for-modes'."
  :type '(repeat symbol) :group 'clojure-pretty-lambda)

;;;###autoload
(defun clojure-pretty-lambda-for-modes (&optional turn-off)
  "Use `clojure-pretty-lambda-mode' for modes in `clojure-pretty-lambda-auto-modes'.
`C-u' to turn off."
  (interactive "P")
  (let (hook-var)
    (cond (turn-off
           (dolist (m  clojure-pretty-lambda-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-on-clojure-pretty-lambda-mode)
             (add-hook hook-var 'turn-off-clojure-pretty-lambda-mode))
           (when (memq major-mode clojure-pretty-lambda-auto-modes)
             (turn-off-clojure-pretty-lambda-mode))) ; Current buffer
          (t
           (dolist (m  clojure-pretty-lambda-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-off-clojure-pretty-lambda-mode)
             (add-hook hook-var 'turn-on-clojure-pretty-lambda-mode))
           (when (memq major-mode clojure-pretty-lambda-auto-modes)
             (turn-on-clojure-pretty-lambda-mode)))))) ; Current buffer

;;;###autoload
(define-minor-mode clojure-pretty-lambda-mode
    "Buffer-local minor mode to display the word `fn' as the Greek letter lambda.
With ARG, turn mode on if ARG is positive, off otherwise."
  :init-value nil
  (cond (clojure-pretty-lambda-mode
         (clojure-pretty-lambda)
         (font-lock-fontify-buffer))
        (t
         (font-lock-remove-keywords
          nil `(("(\\(fn\\)[\[[:space:]]"
                 (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                           ,(make-char 'greek-iso8859-7 107))
                           nil)))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "(\\(fn\\)[\[[:space:]]" nil t)
             (decompose-region (match-beginning 1) (match-end 1)))))))

;;;###autoload
(define-globalized-minor-mode global-clojure-pretty-lambda-mode
    clojure-pretty-lambda-mode turn-on-clojure-pretty-lambda-mode
    "Global minor mode to display the word `fn' as the Greek letter lambda.
With ARG, turn mode on if ARG is positive, off otherwise.")

;; This was originally from <URL: http://www.emacswiki.org/emacs/PrettyLambda>.
;; See that page for the history of this code snippet.  I just added MODE as an
;; optional argument.
(defun clojure-pretty-lambda (&optional mode)
  "Display the word `fn' as the Greek letter lambda.
Non-nil optional arg means use clojure-pretty-lambda display in that MODE.
nil means use clojure-pretty-lambda display for the current mode."
  (font-lock-add-keywords
   mode `(("(\\(fn\\)[\[[:space:]]"
   (0 (progn (compose-region (match-beginning 1) (match-end 1)
        ,(make-char 'greek-iso8859-7 107))
      nil))))))

(defun turn-on-clojure-pretty-lambda-mode  () (clojure-pretty-lambda-mode  1))
(defun turn-off-clojure-pretty-lambda-mode () (clojure-pretty-lambda-mode -1))

;;;;

(provide 'clojure-pretty-lambda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure-pretty-lambda.el ends here
